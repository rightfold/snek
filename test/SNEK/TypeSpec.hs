module SNEK.TypeSpec
( spec
) where

import SNEK.Type
import Test.Hspec

spec :: Spec
spec = do
  describe "==" $ do
    it "UniversalT" $ do
      UniversalT 1 TypeK (VarT 1 TypeK) `shouldBe` UniversalT 1 TypeK (VarT 1 TypeK)
      UniversalT 1 TypeK (VarT 1 TypeK) `shouldBe` UniversalT 2 TypeK (VarT 2 TypeK)
      UniversalT 1 (TypeK *->* TypeK) (UniversalT 2 TypeK (VarT 1 TypeK ~->~ VarT 2 TypeK))
        `shouldBe` UniversalT 2 (TypeK *->* TypeK) (UniversalT 3 TypeK (VarT 2 TypeK ~->~ VarT 3 TypeK))
      UniversalT 1 TypeK (UniversalT 2 TypeK (VarT 1 TypeK ~->~ VarT 2 TypeK))
        `shouldNotBe` UniversalT 2 TypeK (UniversalT 3 TypeK (VarT 3 TypeK ~->~ VarT 2 TypeK))
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
