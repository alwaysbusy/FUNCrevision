import Prelude hiding (and, concat, replicate, (!!), elem, sum, take, last)

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

-- Question 5
msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort xs = merge (msort ys) (msort zs)
    where
            (ys,zs) = halve xs
            halve :: [a] -> ([a], [a])
            halve ys = (take halflen ys, reverse (take (length ys - halflen) (reverse ys)))
                    where
                            halflen = length ys `div` 2

-- Question 6
sum :: Num a => [a] -> a
sum (x:[]) = x
sum (x:xs) = x + sum xs

take :: Int -> [a] -> [a]
take 0 _ = []
take n [] = []
take n (x:xs) = x:(take (n - 1) xs)

last :: [a] -> a
last xs = head (reverse xs)
