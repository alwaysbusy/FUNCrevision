-- Question 1
halve :: [a] -> ([a],[a])
halve xs | length xs `mod` 2 == 0 = (take half xs, reverse (take half (reverse xs)))
    where
        half = length xs `div` 2
halve _ = error("List must be even length")

-- Question 2
safetail :: [a] -> [a]
-- safetail xs = if null xs then xs else tail xs
-- safetail xs
--         | null xs = xs
--         | otherwise = tail xs
safetail [] = []
safetail xs = tail xs

-- Question 3
(\/) :: Bool -> Bool -> Bool
True \/ True = True
True \/ False = True
False \/ True = True
False \/ False = False

-- Question 4
(/\) :: Bool -> Bool -> Bool
-- (/\) a b = if a == True && b == True then True else False

-- Question 5
(/\) a b = if a == True then b else False
