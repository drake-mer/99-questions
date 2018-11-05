module TestQuestions21to30 where

import Questions21to30

import System.Random
import Test.Hspec

testQuestion23 :: IO ()
testQuestion23 = Test.Hspec.hspec $ do
  describe "encode" $ do
    it "extractRandomElem g [1..100]" $
        extractRandomElem (mkStdGen 10) [1..100] `shouldBe` (25, [1..24] ++ [26..100])
  describe "combinations" $ do
    it "combinations 3 [1..3]" $
      combinations 3 [1..3] `shouldBe` [[1, 2, 3]]
    it "combinations 2 [1..3]" $
      combinations 2 [1..3] `shouldBe` [[1, 2], [1, 3], [2, 3]]
    it "combinations 1 [1..3]" $
      combinations 1 [1..3] `shouldBe` [[1], [2], [3]]
