module UtilitySpec where

import Test.Hspec

import Utility

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "break an integer down to digis" $
      toDigits (1234 :: Int) `shouldBe` [1, 2, 3, 4]

  describe "y" $ do
    it "calculate fibonacci number" $ do
      let
        fibF :: (Int -> Int) -> Int -> Int
        fibF _ 0 = 0
        fibF _ 1 = 1
        fibF f n = f (n - 1) + f (n - 2)

        fib = y fibF
      fib 10 `shouldBe` 55
