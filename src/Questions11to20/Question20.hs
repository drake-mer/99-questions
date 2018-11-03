module Questions11to20.Question20 where

-- just do a dumb rotate
removeAt :: [a] -> Int -> [a]
removeAt (x:xs) 1 = xs
removeAt (x:xs) cnt = (x:(removeAt xs (cnt-1)))
removeAt [] _ = []
