module Questions11to20.Question15 where

-- naive implementation
repli [] _ = []
repli (x:xs) integer = (replicate integer x) ++ (repli xs integer)


-- less naive implementation
--
-- helper
rep :: Int -> a -> ([a] -> [a])
rep n x = if (n==0) then id else ((x:).(rep (n-1) x))

-- final function
replic :: Int -> [a] -> [a]
replic n (x:xs) = (rep n x) $ (replic n xs)
replic n [] = []
