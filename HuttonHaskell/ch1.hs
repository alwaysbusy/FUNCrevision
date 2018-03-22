import Prelude hiding (product)

-- Question 3
product :: [Int] -> Int
product (x:xs) = makeProduct x xs
    where
        makeProduct :: Int -> [Int] -> Int
        makeProduct n (x:[]) = n * x
        makeProduct n (x:xs) = makeProduct (n * x) xs

-- Question 4
qsortrev :: Ord a => [a] -> [a]
qsortrev [] = []
qsortrev (x:xs) = qsortrev larger ++ [x] ++ qsortrev smaller
        where
            smaller = [a | a <- xs, a <= x]
            larger = [b | b <- xs, b > x]
