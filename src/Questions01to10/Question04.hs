module Questions01to10.Question04 where 

-- naive implementation
listLength :: [a] -> Integer
listLength list = listLength' 0 list
   where listLength' x (h:xs) = listLength' (x+1) xs
         listLength' x [] = x

-- alternative implementation
listLengthAlt :: [a] -> Integer
listLengthAlt = foldr (\_ y -> y + 1) 0
