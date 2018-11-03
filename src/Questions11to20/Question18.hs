module Questions11to20.Question18 where

slice :: Int -> Int -> [a] -> [a]
slice a b = (drop (a-1)) . (take b)
