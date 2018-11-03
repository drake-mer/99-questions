
module Questions11to20.Question16 where

dropEvery :: [a] -> Integer -> [a]
dropEvery foo nth = [x | (x, y) <- (zip foo [1..]), (y `mod` nth) /= 0]
