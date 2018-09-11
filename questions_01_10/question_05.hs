
-- naive implementation
reverseList :: [a] -> [a]
reverseList list = reverseList' [] list
    where
        reverseList' acc list =
            acc if (null list) else
            reverseList' ((head list):acc) (tail list)

-- With `magic` foldl
reverseList' :: [a] -> [a]
reverseList' = foldl (\acc x  -> (x:acc))  []

-- astounding, isn't it ?
-- This is just using the so called `catamorphism`
-- with cons as operator and [] as first value of the accumulator

