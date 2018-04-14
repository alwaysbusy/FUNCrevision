module P456 where
import P123 hiding (take, drop, zipWith)

-- pascal
pascal :: Tri Integer
pascal = Tri ([1]:[1,1]:(triRows [1,1]))
    where
        triRows :: [Integer] -> [[Integer]]
        triRows xxs = row:(triRows row)
            where
                row = [1] ++ [(xxs!!x) + (xxs!!(x+1)) | x <- [0..((length xxs) - 2)]] ++ [1]

-- hamming
hamming :: [Integer]
hamming = nextHamming 2
    where
        nextHamming :: Integer -> [Integer]
        nextHamming n | length [x | x <- [6..(n-1)], n `mod` x == 0, n `div` x /= 2, n `div` x /= 3, n `div` x /= 5] == 0 && (double || triple || quintuple) = n:(nextHamming (n+1))
                      | otherwise = nextHamming (n+1)
            where
                double = n `mod` 2 == 0
                triple = n `mod` 3 == 0
                quintuple = n `mod` 5 == 0

-- primes
primes :: [Integer]
primes = nextPrime 2
    where
        nextPrime :: Integer -> [Integer]
        nextPrime n | length [x | x <- [2..(n-1)], n `mod` x == 0] == 0 = n:(nextPrime (n+1))
                    | otherwise = nextPrime (n+1)

-- queens
-- skipped due to time constrains

-- edit
-- will return at a later time
