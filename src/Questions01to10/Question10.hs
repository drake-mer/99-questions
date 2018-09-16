module Questions01to10.Question10 where

import qualified Questions01to10.Question09 (pack)

encode :: Eq a => [a] -> [(Int, a)]
encode = (map getCount) . Questions01to10.Question09.pack

getCount :: [a] -> (Int, a)
getCount (x:xs) = ((length (x:xs)), x)
getCount [] = undefined -- should never occur if consistency is up there
