module Questions01to10.Question09 where

subpack :: Eq a => [a] -> ([a], [a])
subpack input = subpack' [] input where
    subpack' anyAcc [] = (anyAcc, [])
    subpack' [] (x:xs) = (subpack' [x] xs)
    subpack' (x:xs) (y:ys) = if (x /= y) then ((x:xs), (y:ys)) else (subpack' (y:x:xs) ys)

pack :: Eq a => [a] -> [[a]]
pack input
    | null first = []
    | null second = [first]
    | otherwise = (first:(pack second))
    where (first, second) = (subpack input)

