module TestQuestions21to30 where

import Questions21to30

import System.Random
import Test.Hspec

testQuestion23 :: IO ()
testQuestion23 = Test.Hspec.hspec $ do
  describe "encode" $ do
    it "extractRandomElem g [1..100]" $
        extractRandomElem (mkStdGen 10) [1..100] `shouldBe` (25, [1..24] ++ [26..100])
