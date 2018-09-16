module TestQuestions01to10 where
import Test.Hspec

import Questions01to10.Question07
import Questions01to10.Question08
import Questions01to10.Question09
import Questions01to10.Question10

testQuestion07 :: IO ()
testQuestion07 = hspec $ do
    describe "flatten" $ do
        it "flatten -> " $
            flatten (List [ Elem 1, List [ List [ (Elem 2), (Elem 3) ],( Elem 4 )]]) `shouldBe` [1, 2, 3, 4]

testQuestion08 :: IO ()
testQuestion08 = hspec $ do
    describe "compress" $ do
        it "" $ compress "aaaaaaaaabccccdddd" `shouldBe` "abcd"

testQuestion09 :: IO ()
testQuestion09 = hspec $ do
  describe "subpack" $ do
    it "\"aaaabcdef\" -> \"aaaa\"" $ subpack "aaaabcdef" `shouldBe` ("aaaa", "bcdef")
    it "\"\" -> \"\"" $ subpack "" `shouldBe` ("", "")
    it "\"ab\" -> \"a\"" $ subpack "ab" `shouldBe` ("a", "b")
    it "\"a\" -> \"a\"" $ subpack "a" `shouldBe` ("a", "")

  describe "pack" $ do
    it "pack is packing" $ pack "aaaabcdeffffg" `shouldBe` ["aaaa", "b", "c", "d", "e", "ffff", "g"]
    it "pack is packing" $ pack "" `shouldBe` []
    it "pack is packing" $ pack "ab" `shouldBe` ["a", "b"]
    it "pack is packing" $ pack "a" `shouldBe` ["a"]

testQuestion10 :: IO ()
testQuestion10 = hspec $ do
  describe "getCount" $ do
      it "\"aaaa\" -> (4, a)" $ getCount "aaaa" `shouldBe` (4, 'a')

  describe "encode" $ do
    it "\"aaaabc\" -> " $
        encode "aaaabc" `shouldBe` [(4, 'a'), (1, 'b'), (1, 'c')]
    it "some test -> " $
        encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
    it "\"\" -> \"\"" $
        encode "" `shouldBe` []
    it "\"ab\" -> \"a\"" $
        encode "ab" `shouldBe`[(1, 'a'), (1, 'b')]
    it "\"a\" -> \"a\"" $
        encode "a" `shouldBe` [(1, 'a')]
