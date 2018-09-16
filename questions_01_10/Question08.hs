-- remove consecutive duplicates

compress :: Eq a => [a] -> [a]
compress (x:y:xs)
    | (x==y) = compress (y:xs)
    | otherwise = x:(compress (y:xs))
compress [x] = [x]

valid :: Bool
valid = ("abcd" == compress "aaaaaaaaabccccdddd")

main :: IO ()
main = do
    putStrLn (show valid)
    return ()
