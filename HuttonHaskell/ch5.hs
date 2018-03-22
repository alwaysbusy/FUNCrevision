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

-- Question 5
twoHalves :: [(Int,Int)]
twoHalves = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]

-- Question 6
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x [(xs!!yi, yi) | yi <- [0..(length xs - 1)]]

-- Question 7
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct as bs = sum [a * b | (a,b) <- zip as bs]

-- Question 8
-- See cipher.hs
