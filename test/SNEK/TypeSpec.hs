module SNEK.TypeSpec
( spec
) where

import SNEK.Type
import Test.Hspec

spec :: Spec
spec = do
  describe "tK" $ do
    it "BoolT" $ do
      tK BoolT `shouldBe` TypeK
    it "FuncT" $ do
      tK FuncT `shouldBe` TypeK *->* TypeK *->* TypeK
    it "ApplyT" $ do
      tK (ApplyT FuncT BoolT) `shouldBe` TypeK *->* TypeK
      tK (ApplyT (ApplyT FuncT BoolT) BoolT) `shouldBe` TypeK
    it "VarT" $ do
      tK (VarT 0 TypeK) `shouldBe` TypeK
