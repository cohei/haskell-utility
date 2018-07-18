module UtilitySpec where

import Test.Hspec

import Utility

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "break an integer down to digis" $
      toDigits (1234 :: Int) `shouldBe` [1, 2, 3, 4]
