module Questions21to30 where

import System.Random
import Data.Maybe

-- question 21
-- Insert an element at a given position into a list.
--
insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt x (y:ys) 0 = (x:y:ys)
insertAt x (y:ys) n = insertAt x (y:ys) (n-1)

-- question 22
-- Create a list containing all integers within a given range.
range :: Enum a => a -> a -> [a]
range a b = [a..b]

-- question 23
--
orderConcat :: [a] -> [a] -> [a]
orderConcat follow (revElem:revFollow) = orderConcat (revElem:follow) revFollow
orderConcat follow [] = follow

extractElem' :: Int -> [a] -> [a] -> StdGen -> (Maybe a, [a])
extractElem' n (x:xs) acc g = 
    if rndNumber == n then (Just x, (orderConcat xs acc)) 
                      else (extractElem' (n-1) xs (x:acc) newG)
    where (rndNumber, newG) = (randomR (1, n) g)

extractRandomElem :: StdGen -> [a] -> (a, [a])
extractRandomElem g input = (fromJust result, newList) 
   where (result, newList) = (extractElem' (length input) input [] g)


extractRandomNElem' :: Int -> StdGen -> [a] -> [a] -> ([a], [a])
extractRandomNElem' 0 _ input output = (input, output)
extractRandomNElem' n g input output = extractRandomNElem' (n-1) newG newIn (rndElem:output)
    where (rndElem, newIn) = extractRandomElem g input
          (_, newG) = System.Random.split g 

extractRandomNElem :: StdGen -> Int  -> [a] -> ([a], [a])
extractRandomNElem g n input = (extractRandomNElem' n g input [])


-- pickRandom: need the length of the input list as a parameter
pickRandom' :: Int -> [a] -> StdGen -> a
pickRandom' n (x:xs) g = if (randomInt == n) then x
                         else (pickRandom' (n-1) xs nextG)
                         where (randomInt, nextG) = (randomR (1, n) g)
pickRandom' 0 _ _ = undefined

pickRandom :: [a] -> StdGen -> a
pickRandom foo g = pickRandom' (length foo) foo g


-- problem 25
--
-- Random Permutations of A List
rnd_permu :: StdGen -> [a] -> [a]
rnd_permu g input = output 
  where (emptyList, output) = extractRandomNElem g (length input) input

-- problem 26
--
-- Combination of K distinct Objects From N Elements In A list
combinations :: Int -> [a] -> [[a]]
combinations _ [] = [[]]
combinations 0 _  = [[]]
combinations k (x:xs) = x_start ++ others
    where x_start = [ x : rest | rest <- combinations (k-1) xs ]
          others  = if k <= length xs then combinations k xs else []
