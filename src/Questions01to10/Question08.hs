module Questions01to10.Question08 where 
-- remove consecutive duplicates
--
--

compress :: Eq a => [a] -> [a]
compress (x:y:xs)
    | (x==y) = compress (y:xs)
    | otherwise = x:(compress (y:xs))
compress [x] = [x]
