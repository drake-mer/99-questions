module Questions01to10.Question06 where 

import Questions01to10.Question05

isPalindrom :: Eq a => [a] -> Bool
isPalindrom = \x -> ((reverseList x) == x)
