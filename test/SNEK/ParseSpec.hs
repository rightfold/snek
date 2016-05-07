{-# LANGUAGE LambdaCase #-}
module SNEK.ParseSpec
( spec
) where

import SNEK.AST (TE(..), VE(..))
import SNEK.Data (Datum(..))
import SNEK.Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "parseTE NameVE" $ do
    it "ok" $ do
      parseTE (Symbol "x") `shouldSatisfy` \case
        Right (NameTE () "x") -> True
        _ -> False
  describe "parseVE NameVE" $ do
    it "ok" $ do
      parseVE (Symbol "x") `shouldSatisfy` \case
        Right (NameVE () "x") -> True
        _ -> False
  describe "parseVE ValueApplyVE" $ do
    it "ok" $ do
      parseVE (List []) `shouldSatisfy` \case
        Left CallWithoutCallee -> True
        _ -> False
      parseVE (List [Symbol "x"]) `shouldSatisfy` \case
        Left CallWithoutArgument -> True
        _ -> False
      parseVE (List [Symbol "x", Symbol "y"]) `shouldSatisfy` \case
        Right (ValueApplyVE (NameVE () "x") (NameVE () "y")) -> True
        _ -> False
      parseVE (List [Symbol "x", Symbol "y", Symbol "z"]) `shouldSatisfy` \case
        Right (ValueApplyVE (ValueApplyVE (NameVE () "x") (NameVE () "y"))
                            (NameVE () "z")) -> True
        _ -> False
  describe "parseVE ValueApplyTE" $ do
    it "ok" $ do
      parseVE (Array []) `shouldSatisfy` \case
        Left CallWithoutCallee -> True
        _ -> False
      parseVE (Array [Symbol "x"]) `shouldSatisfy` \case
        Left CallWithoutArgument -> True
        _ -> False
      parseVE (Array [Symbol "x", Symbol "y"]) `shouldSatisfy` \case
        Right (TypeApplyVE (NameVE () "x") (NameTE () "y")) -> True
        _ -> False
      parseVE (Array [Symbol "x", Symbol "y", Symbol "z"]) `shouldSatisfy` \case
        Right (TypeApplyVE (TypeApplyVE (NameVE () "x") (NameTE () "y"))
                           (NameTE () "z")) -> True
        _ -> False
