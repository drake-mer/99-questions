module Questions11to20.Question11 where

import Data.List
import qualified Questions01to10.Question09 (pack)

data EncodedSymbol a = Single a | Count Int a deriving (Show, Eq)

encode :: Eq a => [a] -> [EncodedSymbol a]
encode = (map codeLetter) . Questions01to10.Question09.pack

codeLetter :: Eq a => [a] -> EncodedSymbol a
codeLetter (x:xs) = if null xs then (Single x) else (Count ((length xs) + 1) x)
codeLetter [] = undefined -- should never occur if consistency is up there
