
-- naive implementation
listLength :: [a] -> Integer
listLength list = listLength' 0 list
   where listLength' x (h:xs) = listLength' (x+1) xs
         listLength' x [] = x

-- Problem: evaluation is hell here
-- Solution: use foldr
--
