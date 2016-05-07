module SNEK.ReadSpec
( spec
) where

import SNEK.Data (Datum(..))
import SNEK.Read
import Test.Hspec

spec :: Spec
spec = do
  describe "readData" $ do
    it "ok" $ do
      readData "" `shouldBe` Just []
      readData "a" `shouldBe` Just [Symbol "a"]
      readData "()" `shouldBe` Just [List []]
      readData "[]" `shouldBe` Just [Array []]
      readData "(a b)" `shouldBe` Just [List [Symbol "a", Symbol "b"]]
      readData "[a b]" `shouldBe` Just [Array [Symbol "a", Symbol "b"]]
      readData "[] () a" `shouldBe` Just [Array [], List [], Symbol "a"]
