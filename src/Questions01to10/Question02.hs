module Questions01to10.Question02 where

myButLast :: [a] -> a
myButLast (x:y:xs) = if (null xs) then x else last (y:xs)
myButLast (x:[]) = undefined
myButLast [] = undefined
