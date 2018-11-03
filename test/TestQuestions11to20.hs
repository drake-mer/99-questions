module TestQuestions11to20 where

import Questions11to20.Question11
import Questions11to20.Question12
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

testQuestion12 :: IO ()
testQuestion12 = Test.Hspec.hspec $ do
  it "decodeModified [(Count 4 'a'), (Single 'b')]" $ do 
      decodeModified [(Count 4 'a'), (Single 'b')] `shouldBe` "aaaab"
  it "decodeModified [(Single 'c'), (Single 'b')]" $ do 
      decodeModified [(Single 'c'), (Single 'b')] `shouldBe` "cb"
  it "decodeModified []" $ do decodeModified [] `shouldBe` ""

