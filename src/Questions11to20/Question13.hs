module Questions11to20.Question13 where


data CharStore a = Null | CharStore Int a deriving (Show, Eq)

encodeDirect :: Eq a => [a] -> [CharStore a]
encodeDirect = encodeDirect' Null

encodeDirect' :: Eq a => CharStore a -> [a] -> [CharStore a]
encodeDirect' Null [] = []
encodeDirect' (CharStore u x) [] = [(CharStore u x)]
encodeDirect' Null (x:xs) = (encodeDirect' (CharStore 1 x) xs)
encodeDirect' (CharStore u x) (y:ys)
    | x /= y = (CharStore u x):(encodeDirect' Null (y:ys))
    | x == y = (encodeDirect' (CharStore (u+1) x) ys)
