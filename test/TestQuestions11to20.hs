module TestQuestions11to20 where

import Questions11to20.Question11
import Test.Hspec

testQuestion11 :: IO ()
testQuestion11 = Test.Hspec.hspec $ do
  describe "encode" $ do
    it "\"aaaabc\" -> " $
        encode "aaaabc" `shouldBe` [(Count 4 'a'), (Single 'b'), (Single 'c')]
    it "some test -> " $
        encode "aaaabccaadeeee" `shouldBe` [
            (Count 4 'a')
            , (Single 'b')
            , (Count 2 'c')
            , (Count 2 'a')
            , (Single 'd')
            , (Count 4 'e')
        ]
