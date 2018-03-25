-- Question 4
-- For sake of study, done using recursion instead of list comprehension
fibs :: [Integer]
fibs = 0:1:(genFib 0 1)
    where
        genFib :: Integer -> Integer -> [Integer]
        genFib x y = (x+y):(genFib y (x+y))

-- Question 5
fib :: Int -> Integer
fib n = fibs!!n

fibthousand :: Integer
fibthousand = nfib 0
        where
            nfib :: Int -> Integer
            nfib n
                | fibv > 1000 = fibv
                | otherwise = nfib (n + 1)
                where
                    fibv = fib n
