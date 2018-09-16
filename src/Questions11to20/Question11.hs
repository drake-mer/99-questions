import qualified Questions01to10.Question09 (pack)
import Test.Hspec

encode :: Eq a => [a] -> [(Int, a)]
encode = (map getCount) . Questions01to10.Question09.pack

getCount :: [a] -> (Int, a)
getCount (x:xs) = ((length (x:xs)), x)
getCount [] = undefined -- should never occur if consistency is up there

main :: IO ()
main = hspec $ do
  describe "getCount" $ do
      it "\"aaaa\" -> " $ getCount "aaaa" `shouldBe` (4, 'a')

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
