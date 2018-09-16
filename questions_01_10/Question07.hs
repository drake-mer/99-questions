-- Flatten a data structure
-- flatten [[[1]]] = [1]

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List ((Elem x):xs)) = x:(flatten (List xs))
flatten (List ((List x):xs)) = (flatten (List x)) ++ (flatten (List xs))

-- list flattening example
valid = [1, 2, 3, 4] == flatten (List [ Elem 1, List [ List [ (Elem 2), (Elem 3) ],( Elem 4 )]])
