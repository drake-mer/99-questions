module Questions11to20.Question14 where 

duplicateElems :: [a] -> [a]
duplicateElems [] = []
duplicateElems (x:xs) = (x:x:(duplicateElems xs))

