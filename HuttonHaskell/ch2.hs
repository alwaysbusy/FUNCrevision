import Prelude hiding (last, init)

-- Question 2
double :: Int -> Int
double x = x + x

quadruple :: Int -> Int
quadruple x = double (double x)

factorial :: Int -> Int
factorial n = product [1..n]

average :: [Int] -> Int
average ns = sum ns `div` length ns

-- Question 3
func = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- Question 4
last :: [a] -> a
last xs = head (reverse xs)

-- Question 5
init :: [a] -> [a]
--init xs = reverse (tail (reverse xs))
init xs = take (length xs - 1) xs
