module Questions01to10.Question05 where 

-- With `magic` foldl
reverseList :: [a] -> [a]
reverseList = foldl (\acc x  -> (x:acc))  []
