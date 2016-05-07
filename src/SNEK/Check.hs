{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
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

  -- * Checking expressions
, checkTE
, checkVE
) where

import Control.Category ((>>>))
import Control.Lens ((%~), makeLenses, view)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (local, Reader)
import Control.Monad.Trans.Either (EitherT)
import Data.Map (Map)
import SNEK.AST (KE(..), TE(..), VE(..))
import SNEK.Symbol (VS(..), KS(..), TS(..))
import SNEK.Type (K(..), T(..), tK)

import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- Environment
-------------------------------------------------------------------------------

data E = E { _eKSs :: Map String KS
           , _eTSs :: Map String TS
           , _eVSs :: Map String VS
           }
$(makeLenses ''E)

emptyE :: E
emptyE = E Map.empty Map.empty Map.empty

-------------------------------------------------------------------------------
-- Infrastructure
-------------------------------------------------------------------------------

data CheckError
  = TypeNotInScope String
  | ValueNotInScope String
  | KindMismatch K K
  deriving (Eq, Show)

type Check = EitherT CheckError (Reader E)

-------------------------------------------------------------------------------
-- Checking expressions
-------------------------------------------------------------------------------

checkTE :: TE ts -> Check (TE TS)
checkTE (NameTE _ name) = (view eTSs >>=) $ Map.lookup name >>> \case
                            Just ts -> return $ NameTE ts name
                            Nothing -> throwError (TypeNotInScope name)

checkVE :: VE ks ts vs -> Check (VE KS TS VS)
checkVE (NameVE _ name) = (view eVSs >>=) $ Map.lookup name >>> \case
                            Just vs -> return $ NameVE vs name
                            Nothing -> throwError (ValueNotInScope name)
checkVE (ValueLambdaVE p pt b) = do
  pt' <- checkTE pt
  let ptT = teT pt'
  expectKind (tK ptT) TypeK
  b' <- local (eVSs %~ Map.insert p (VS ptT)) $ checkVE b
  return $ ValueLambdaVE p pt' b'

-------------------------------------------------------------------------------
-- Deriving kinds and types from expressions
-------------------------------------------------------------------------------

teT :: TE TS -> T
teT (NameTE t _) = tsT t

-------------------------------------------------------------------------------
-- Error helpers
-------------------------------------------------------------------------------

expectKind :: K -> K -> Check ()
expectKind k l | k == l    = return ()
               | otherwise = throwError (KindMismatch k l)
