import Prelude hiding (all, any, takeWhile, dropWhile)

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
