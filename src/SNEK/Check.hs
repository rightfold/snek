{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module SNEK.Check
( -- * Environment
  E(..)
, eKSs
, eTSs
, eVSs
, emptyE

  -- * Infrastructure
, CheckError(..)
, Check
, runCheck

  -- * Checking expressions
, checkTE
, checkVE

  -- * Deriving kinds and types from expressions
, keK
, teT
, veT
) where

import Control.Category ((>>>))
import Control.Lens ((%~), makeLenses, view)
import Control.Monad (forM)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (local, ReaderT, runReaderT)
import Control.Monad.State (evalState, get, modify, State)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Map (Map)
import SNEK.AST (KE(..), TE(..), VE(..))
import SNEK.Symbol (KS(..), TS(..), VS(..))
import SNEK.Type (K(..), replaceVarT, T(..), tK)
import System.FilePath ((</>), (<.>), takeDirectory)

import qualified Data.Map as Map
import qualified System.FilePath as FilePath

-------------------------------------------------------------------------------
-- Environment
-------------------------------------------------------------------------------

-- | Environment (also known as "symbol table").
data E = E { _eKSs      :: Map String KS -- ^ Kind symbols in scope.
           , _eTSs      :: Map String TS -- ^ Type symbols in scope.
           , _eVSs      :: Map String VS -- ^ Value symbols in scope.
           , _file      :: String
           , _fileTypes :: Map String T
           }
$(makeLenses ''E)

-- | Empty environment.
emptyE :: String -> Map String T -> E
emptyE = E Map.empty Map.empty Map.empty

-------------------------------------------------------------------------------
-- Infrastructure
-------------------------------------------------------------------------------

data CheckError
  = KindNotInScope String
  | TypeNotInScope String
  | ValueNotInScope String
  | KindMismatch K K
  | TypeMismatch T T
  | NonFunctionApplication T
  | NonStruct T
  | UnknownField String T
  | NoSuchFile String
  | InferenceFailure T T
  deriving (Eq, Show)

type Check = EitherT CheckError (ReaderT E (State Int))

runCheck :: Check a -> E -> Either CheckError a
runCheck c e = evalState (runReaderT (runEitherT c) e) 0

fresh :: Check Int
fresh = do { id <- get; modify (+ 1); return id }

-------------------------------------------------------------------------------
-- Checking expressions
-------------------------------------------------------------------------------

-- | Check a kind expression in the current environment.
checkKE :: KE ts -> Check (KE KS)
checkKE (NameKE _ name) = (view eKSs >>=) $ Map.lookup name >>> \case
                            Just ks -> return $ NameKE ks name
                            Nothing -> throwError (KindNotInScope name)
checkKE (ApplyKE f a) = do
  f' <- checkKE f
  a' <- checkKE a
  -- TODO: sort-check kinds
  return $ ApplyKE f' a'

-- | Check a type expression in the current environment.
checkTE :: TE ts -> Check (TE TS)
checkTE (NameTE _ name) = (view eTSs >>=) $ Map.lookup name >>> \case
                            Just ts -> return $ NameTE ts name
                            Nothing -> throwError (TypeNotInScope name)
checkTE (StructTE fs) = StructTE <$> mapM checkTE fs
checkTE (ApplyTE f a) = do
  f' <- checkTE f
  a' <- checkTE a
  -- TODO: kind-check types
  return $ ApplyTE f' a'

-- | Check a value expression in the current environment.
checkVE :: VE ks ts vs -> Check (VE KS TS VS)
checkVE (NameVE _ name) = (view eVSs >>=) $ Map.lookup name >>> \case
                            Just vs -> return $ NameVE vs name
                            Nothing -> throwError (ValueNotInScope name)
checkVE (BoolVE b) = return $ BoolVE b
checkVE (StructVE fs) = StructVE <$> mapM checkVE fs
checkVE (StructReadVE f s) = do
  s' <- checkVE s
  let sT = veT s'
  case sT of
    StructT fs | Map.member f fs -> return $ StructReadVE f s'
               | otherwise       -> throwError (UnknownField f sT)
    _ -> throwError (NonStruct sT)
checkVE (IfVE c t f) = do
  c' <- checkVE c
  expectType (veT c') BoolT
  t' <- checkVE t
  f' <- checkVE f
  expectType (veT t') (veT f')
  return $ IfVE c' t' f'
checkVE (LetVE n v b) = do
  v' <- checkVE v
  let vT = veT v'
  b' <- local (eVSs %~ Map.insert n (VS vT)) $ checkVE b
  return $ LetVE n v' b'
checkVE (LetRecVE bds b) = do
  bds' <- forM bds $ \(n, t, v) -> (n, , v) <$> checkTE t
  let bVSs = foldl (\m (n, t', _) -> Map.insert n (VS (teT t')) m) Map.empty bds'
  local (eVSs %~ Map.union bVSs) $ do
    bds'' <- forM bds' $ \(n, t', v) -> do
               v' <- checkVE v
               expectType (veT v') (teT t')
               return (n, t', v')
    b' <- checkVE b
    return $ LetRecVE bds'' b'
checkVE (ValueLambdaVE p pt b) = do
  pt' <- checkTE pt
  let ptT = teT pt'
  expectKind (tK ptT) TypeK
  b' <- local (eVSs %~ Map.insert p (VS ptT)) $ checkVE b
  return $ ValueLambdaVE p pt' b'
checkVE (TypeLambdaVE p _ pk b) = do
  pk' <- checkKE pk
  varID <- fresh
  let pT = VarT varID (keK pk')
  b' <- local (eTSs %~ Map.insert p (TS pT)) $ checkVE b
  return $ TypeLambdaVE p varID pk' b'
checkVE (ValueApplyVE f a) = do
  f' <- checkVE f
  case veT f' of
    ApplyT (ApplyT FuncT pt) _ -> do
      a' <- checkVE a
      let at = veT a'
      if at == pt
        then return $ ValueApplyVE f' a'
        else throwError (TypeMismatch pt at)
    t -> throwError (NonFunctionApplication t)
checkVE (TypeApplyVE f a) = do
  f' <- checkVE f
  case veT f' of
    UniversalT _ k _ -> do
      a' <- checkTE a
      let ak = tK (teT a')
      if ak == k
        then return $ TypeApplyVE f' a'
        else throwError (KindMismatch ak k)
    t -> throwError (NonFunctionApplication t)
checkVE (ImportVE _ importFile) = do
  curDir <- takeDirectory <$> view file
  let absoluteImportFile = FilePath.normalise $ curDir </> importFile <.> "snek"
  it <- Map.lookup absoluteImportFile <$> view fileTypes
  case it of
    Just t  -> return (ImportVE (VS t) importFile)
    Nothing -> throwError (NoSuchFile absoluteImportFile)

-------------------------------------------------------------------------------
-- Deriving kinds and types from expressions
-------------------------------------------------------------------------------

-- | Return the kind of a kind expression. The kind expression must be
--   well-kinded.
keK :: KE KS -> K
keK (NameKE ks _) = ksK ks
keK (ApplyKE f a) = ApplyK (keK f) (keK a)

-- | Return the type of a type expression. The type expression must be
--   well-typed.
teT :: TE TS -> T
teT (NameTE ts _) = tsT ts
teT (StructTE fs) = StructT (fmap teT fs)
teT (ApplyTE f a) = ApplyT (teT f) (teT a)

-- | Return the type of a value expression. The value expression must be
--   well-typed.
veT :: VE KS TS VS -> T
veT (NameVE vs _) = vsT vs
veT (BoolVE _) = BoolT
veT (StructVE fs) = StructT (fmap veT fs)
veT (StructReadVE f s) =
  case veT s of
    StructT fs -> case Map.lookup f fs of
                    Just t  -> t
                    Nothing -> error "veT: ill-typed expression"
    _ -> error "veT: ill-typed expression"
veT (IfVE _ t _) = veT t
veT (LetVE _ _ b) = veT b
veT (LetRecVE _ b) = veT b
veT (ValueLambdaVE _   pt b) = ApplyT (ApplyT FuncT (teT pt)) (veT b)
veT (TypeLambdaVE  _ i pk b) = UniversalT i (keK pk) (veT b)
veT (ValueApplyVE f _) =
  case veT f of
    ApplyT (ApplyT FuncT _) r -> r
    _ -> error "veT: ill-typed expression"
veT (TypeApplyVE f a) =
  case veT f of
    UniversalT i _ b -> replaceVarT i b (teT a)
    _ -> error "veT: ill-typed expression"
veT (ImportVE s _) = vsT s

-------------------------------------------------------------------------------
-- Error helpers
-------------------------------------------------------------------------------

expectKind :: K -> K -> Check ()
expectKind k l | k == l    = return ()
               | otherwise = throwError (KindMismatch k l)

expectType :: T -> T -> Check ()
expectType t u | t == u    = return ()
               | otherwise = throwError (TypeMismatch t u)
