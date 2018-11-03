module Questions11to20.Question12 where
import Questions11to20.Question11

decodeModified :: Eq a => [EncodedSymbol a] -> [a]
decodeModified ((Single u):xs) = (u:(decodeModified xs))
decodeModified ((Count x u):xs) = (take x (repeat u)) ++ (decodeModified xs)
decodeModified [] = []
