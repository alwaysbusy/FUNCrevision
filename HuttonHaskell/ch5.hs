import Prelude hiding (replicate)

-- Question 1
bigsum :: Int
bigsum = sum [x ^ 2 | x <- [1..100]]

-- Question 2
replicate :: Int -> a -> [a]
replicate n x = [x | y <- [1..n]]

-- Question 3
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], (x ^ 2) + (y ^ 2) == (z ^ 2)]

-- Question 4
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) - x == x]
