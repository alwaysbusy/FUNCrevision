import Prelude hiding (all, any, takeWhile, dropWhile, map, filter)
import transmit

-- Question 1
q1 f p xs = map f (filter p xs)

-- Question 2
all :: (a -> Bool) -> [a] -> Bool
all f (x:[]) = f x
all f (x:xs) = if f x == True then all f xs else False

any :: (a -> Bool) -> [a] -> Bool
any f (x:[]) = f x
any f (x:xs) = if f x == True then True else any f xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs) = if f x == True then x:(takeWhile f xs) else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f xxs@(x:xs) = if f x == True then dropWhile f xs else xxs

-- Question 3
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> (f x):xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x xs -> if f x then x:xs else xs) []

-- Question 4
dec2int :: [Int] -> Int
dec2int = foldl (\xs x -> (xs * 10) + x) 0

-- Question 7
unfold p h t x
    | p x = []
    | otherwise = h x : unfold p h t (t x)

int2bin = unfold (==0) (`mod`2) (`div`2)

type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (null) (take 8) (drop 8)

iterate :: (a -> a) -> a -> [a]
iterate f = unfold (null) f f

-- Question 8 and 9
-- See transmit.hs
