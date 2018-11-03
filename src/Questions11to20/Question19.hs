module Questions11to20.Question19 where

-- just do a dumb rotate
rotate :: [a] -> Int -> [a]
rotate list n = [ x | (x, y) <- (zip (drop n (cycle list)) list)]
