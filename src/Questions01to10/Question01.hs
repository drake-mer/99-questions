
-- Question 1
--
myLast :: [a] -> a
myLast (x:xs) = if (null xs) then x else last xs
myLast [] = undefined
