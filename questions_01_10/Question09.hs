module Question09 where

import Test.Hspec

subpack :: Eq a => [a] -> ([a], [a])
subpack input = subpack' [] input where
    subpack' anyAcc [] = (anyAcc, [])
    subpack' [] (x:xs) = (subpack' [x] xs)
    subpack' (x:xs) (y:ys) = if (x /= y) then ((x:xs), (y:ys)) else (subpack' (y:x:xs) ys)

pack :: Eq a => [a] -> [[a]]
pack input
    | null first = []
    | null second = [first]
    | otherwise = (first:(pack second))
    where (first, second) = (subpack input)


main :: IO ()
main = hspec $ do
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


