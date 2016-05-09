{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module SNEK.Parse
( ParseError(..)

, parseKE
, parseTE
, parseVE
) where

import Control.Monad (forM, when)
import Data.List.Split (chunksOf)
import SNEK.AST (KE(..), TE(..), VE(..))
import SNEK.Data (Datum(..))

import qualified Data.Map as Map

data ParseError
  = CallWithoutCallee
  | CallWithoutArgument
  | NonSymbolStructKey
  | IllFormedSpecialForm
  deriving (Eq, Show)

-- | Turn a datum into a kind expression.
parseKE :: Datum -> Either ParseError (KE ())
parseKE (Symbol name) = return $ NameKE () name
parseKE (List (f : as@(_ : _))) = do
  f' <- parseKE f
  as' <- mapM parseKE as
  return $ foldl ApplyKE f' as'
parseKE _ = error "not yet implemented"

-- | Turn a datum into a type expression.
parseTE :: Datum -> Either ParseError (TE ())
parseTE (Symbol name) = return $ NameTE () name
parseTE (List (f : as@(_ : _))) = do
  f' <- parseTE f
  as' <- mapM parseTE as
  return $ foldl ApplyTE f' as'
parseTE (Dict fs) = do
  fs' <- (Map.fromList <$>) . forM (Map.toList fs) $ \case
    (Symbol n, t) -> (n,) <$> parseTE t
    _ -> throwError NonSymbolStructKey
  return (StructTE fs')
parseTE _ = error "not yet implemented"

-- | Turn a datum into a value expression.
parseVE :: Datum -> Either ParseError (VE () () ())
parseVE (Symbol name)    = return $ NameVE () name
parseVE (Bool b)         = return $ BoolVE b
parseVE (List  [])       = throwError CallWithoutCallee
parseVE (Array [])       = throwError CallWithoutCallee
parseVE (List  [_])      = throwError CallWithoutArgument
parseVE (Array [_])      = throwError CallWithoutArgument
parseVE (List  (f : as)) = do
  special <- parseVESpecial f as
  case special of
    Just ve -> return ve
    Nothing -> do
      f' <- parseVE f
      as' <- mapM parseVE as
      return $ foldl ValueApplyVE f' as'
parseVE (Array (f : as)) = do
  f' <- parseVE f
  as' <- mapM parseTE as
  return $ foldl TypeApplyVE f' as'
parseVE (Dict fs) = StructVE . Map.fromList <$> go
  where go = forM (Map.toList fs) $ \case
               (Symbol n, v) -> (n,) <$> parseVE v
               _ -> throwError NonSymbolStructKey

parseVESpecial :: Datum -> [Datum] -> Either ParseError (Maybe (VE () () ()))
parseVESpecial (Symbol ('.' : f)) as = Just <$> go
  where go = case as of
               [v] -> StructReadVE f <$> parseVE v
               _ -> throwError IllFormedSpecialForm
parseVESpecial (Symbol "if") as = Just <$> go
  where go = case as of
               [c, t, f] -> IfVE <$> parseVE c <*> parseVE t <*> parseVE f
               _ -> throwError IllFormedSpecialForm
parseVESpecial (Symbol "let") as = Just <$> go
  where go = case as of
              [List bds, b] -> do
                bds' <- forM (chunksOf 2 bds) $ \case
                          [Symbol n, v] -> (n,) <$> parseVE v
                          _ -> throwError IllFormedSpecialForm
                b' <- parseVE b
                return $ foldr (uncurry LetVE) b' bds'
              _ -> throwError IllFormedSpecialForm
parseVESpecial (Symbol "let-rec") as = Just <$> go
  where go = case as of
              [List bds, b] -> do
                bds' <- forM (chunksOf 3 bds) $ \case
                          [Symbol n, t, v] -> (n,,) <$> parseTE t <*> parseVE v
                          _ -> throwError IllFormedSpecialForm
                b' <- parseVE b
                return $ LetRecVE bds' b'
              _ -> throwError IllFormedSpecialForm
parseVESpecial (Symbol "fn") as = Just <$> go
  where go = case as of
          [List  ps, b] -> go' ValueLambdaVE           ps b parseTE
          [Array ps, b] -> go' (TypeLambdaVE `flip` 0) ps b parseKE
          _ -> throwError IllFormedSpecialForm
        go' node ps b parseTK = do
          ps' <- parseParams ps parseTK
          b' <- parseVE b
          return $ foldr (uncurry node) b' ps'
        parseParams ps parseTK = do
          let pairs = (chunksOf 2 ps)
          when (null pairs) (throwError IllFormedSpecialForm)
          forM pairs $ \case
            [Symbol n, t] -> (n,) <$> parseTK t
            _ -> throwError IllFormedSpecialForm
parseVESpecial (Symbol "import") as = Just <$> go
  where go = case as of
               [String file] -> return $ ImportVE () file
               _ -> throwError IllFormedSpecialForm
parseVESpecial _ _ = return Nothing

throwError :: a -> Either a b
throwError = Left
