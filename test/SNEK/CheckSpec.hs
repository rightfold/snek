{-# LANGUAGE LambdaCase #-}
module SNEK.CheckSpec
( spec
) where

import Control.Lens ((%~))
import Control.Monad.Reader (runReader)
import Control.Monad.Trans.Either (runEitherT)
import Data.Function ((&))
import SNEK.AST (TE(..), VE(..))
import SNEK.Check
import SNEK.Symbol (TS(..), VS(..))
import SNEK.Type ((*->*), (~->~), K(..), T(..))
import Test.Hspec

import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "checkTE NameTE" $ do
    it "ok" $ do
      let c = checkTE (NameTE () "t")
      let e = emptyE & eTSs %~ Map.insert "t" (TS FuncT)
      runReader (runEitherT c) e `shouldSatisfy` \case
        Right (NameTE ts "t") -> tsT ts == FuncT
        _ -> False
    it "TypeNotInScope" $ do
      let c = checkTE (NameTE () "t")
      runReader (runEitherT c) emptyE `shouldSatisfy` \case
        Right _ -> False
        Left er -> er == TypeNotInScope "t"
  describe "checkVE NameVE" $ do
    it "ok" $ do
      let c = checkVE (NameVE () "x")
      let e = emptyE & eVSs %~ Map.insert "x" (VS BoolT)
      runReader (runEitherT c) e `shouldSatisfy` \case
        Right (NameVE vs "x") -> vsT vs == BoolT
        _ -> False
    it "ValueNotInScope" $ do
      let c = checkVE (NameVE () "x")
      runReader (runEitherT c) emptyE `shouldSatisfy` \case
        Right _ -> False
        Left er -> er == ValueNotInScope "x"
  describe "checkVE ValueLambdaVE" $ do
    it "ok" $ do
      let c = checkVE (ValueLambdaVE "x" (NameTE () "t") (NameVE () "x"))
      let e = emptyE & eTSs %~ Map.insert "t" (TS BoolT)
      runReader (runEitherT c) e `shouldSatisfy` \case
        Right (ValueLambdaVE "x" (NameTE ts "t") (NameVE vs "x")) ->
          tsT ts == BoolT && vsT vs == BoolT
        _ -> False
    it "KindMismatch" $ do
      let c = checkVE (ValueLambdaVE "x" (NameTE () "t") (NameVE () "x"))
      let e = emptyE & eTSs %~ Map.insert "t" (TS FuncT)
      runReader (runEitherT c) e `shouldSatisfy` \case
        Right _ -> False
        Left er -> er == KindMismatch (TypeK *->* TypeK *->* TypeK) TypeK
  describe "checkVE ValueApplyVE" $ do
    it "ok" $ do
      let c = checkVE (ValueApplyVE (NameVE () "f") (NameVE () "x"))
      let e = emptyE
              & eVSs %~ Map.insert "f" (VS (BoolT ~->~ BoolT ~->~ BoolT))
              & eVSs %~ Map.insert "x" (VS BoolT)
      runReader (runEitherT c) e `shouldSatisfy` \case
        Right (ValueApplyVE (NameVE fVS "f") (NameVE xVS "x")) ->
             vsT fVS == BoolT ~->~ BoolT ~->~ BoolT
          && vsT xVS == BoolT
        _ -> False
    it "NonFunctionApplication" $ do
      let c = checkVE (ValueApplyVE (NameVE () "f") (NameVE () "x"))
      let e = emptyE
              & eVSs %~ Map.insert "f" (VS BoolT)
              & eVSs %~ Map.insert "x" (VS BoolT)
      runReader (runEitherT c) e `shouldSatisfy` \case
        Right _ -> False
        Left er -> er == NonFunctionApplication BoolT
    it "TypeMismatch" $ do
      let c = checkVE (ValueApplyVE (NameVE () "f") (NameVE () "x"))
      let e = emptyE
              & eVSs %~ Map.insert "f" (VS ((BoolT ~->~ BoolT) ~->~ BoolT))
              & eVSs %~ Map.insert "x" (VS BoolT)
      runReader (runEitherT c) e `shouldSatisfy` \case
        Right _ -> False
        Left er -> er == TypeMismatch (BoolT ~->~ BoolT) BoolT
