module Questions11to20.Question17 where

split :: [a] -> Int -> ([a], [a])
split a c = (first, second) 
    where first = firstPart a c
          second = secondPart a c

firstPart [] _ = []
firstPart (x:xs) c = if (c==0) then [] else (x:(firstPart xs (c-1)))

secondPart [] _ = []
secondPart (x:xs) c = if (c==0) then (x:xs) else (secondPart xs (c-1))




