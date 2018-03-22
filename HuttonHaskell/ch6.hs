import Prelude hiding (and, concat, replicate, (!!), elem)

-- Question 3
and :: [Bool] -> Bool
and (False:_) = False
and (True:[]) = True
and (True:xs) = and xs

concat :: [[a]] -> [a]
concat xs = adding [] xs
    where
        adding :: [a] -> [[a]] -> [a]
        adding u (v:[]) = u ++ v
        adding u (v:vs) = adding (u ++ v) vs

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n e = e:(replicate (n-1) e)

(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(x:xs) !! n = xs!!(n-1)

elem :: Eq a => a -> [a] -> Bool
elem a (b:_) | a == b = True
elem _ [] = False
elem a (b:bs) = elem a bs

-- Question 4
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge (x:xs) [] = x:(merge xs [])
merge [] (y:ys) = y:(merge [] ys)
merge xss@(x:xs) yss@(y:ys)
        | x <= y = x:(merge xs yss)
        | otherwise = y:(merge xss ys)
