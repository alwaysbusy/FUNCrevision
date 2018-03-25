-- Question 1
choices :: Eq a => [a] -> [[a]]
choices [] = [[]]
choices xs = flatten [perms ys | ys <- subs xs]
    where
        perms :: Eq a => [a] -> [[a]]
        perms [] = [[]]
        perms xs = [x:ys | x <- xs, ys <- perms (filter (/=x) xs)]
        subs :: [a] -> [[a]]
        subs [] = [[]]
        subs (x:xs) = yss ++ map (x:) yss
            where
                yss = subs xs
                
flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

-- Question 2
-- For sake of time (and my study purposes) a much simpler solution
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice ch eq = ch `elem` (choices eq)
