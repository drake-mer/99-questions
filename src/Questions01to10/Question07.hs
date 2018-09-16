module Questions01to10.Question07 where 
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List ((Elem x):xs)) = x:(flatten (List xs))
flatten (List ((List x):xs)) = (flatten (List x)) ++ (flatten (List xs))

