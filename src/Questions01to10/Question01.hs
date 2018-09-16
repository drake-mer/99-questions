-- give last element of a list
module Questions01to10.Question01 where

myLast :: [a] -> a
myLast (x:xs) = if (null xs) then x else myLast xs
myLast [] = undefined
