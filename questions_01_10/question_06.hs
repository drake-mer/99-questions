-- isPalindrom ?
reverseList' :: [a] -> [a]
reverseList' = foldl (\acc x  -> (x:acc))  []

isPalindrom :: [a] -> Bool
isPalindrom = \x -> ((reverseList x) == x)
