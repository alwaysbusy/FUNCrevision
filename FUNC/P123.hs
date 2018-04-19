module P123 where
import Prelude hiding (take, drop, zipWith)

-- take, drop
take, drop :: Int -> [a] -> [a]

take 0 _ = []
take n (x:xs) | n > 0 = x:(take (n-1) xs)

drop 0 xs = xs
drop n (x:xs) | n > 0 = drop (n-1) xs

-- positions
positions :: Eq a => [a] -> a -> [Int]
positions xxs y = posfinder xxs y 0
    where
        posfinder :: Eq a => [a] -> a -> Int -> [Int]
        posfinder [] _ _ = []
        posfinder (x:xs) y n | x == y = n:(posfinder xs y (n+1))
                             | otherwise = posfinder xs y (n+1)

-- duplicates
duplicates :: Eq a => [a] -> [a]
duplicates xxs = dupfinder xxs [] []
    where
        dupfinder :: Eq a => [a] -> [a] -> [a] -> [a]
        dupfinder [] _ zzs = zzs
        dupfinder (x:xs) ys zs = if (x `elem` ys) then
                                    dupfinder xs ys (x:zs)
                                else
                                    dupfinder xs (x:ys) zs

-- sort
sort :: Ord a => [a] -> [a]
sort [] = []
sort [a] = [a]
sort xxs = merge (sort (take (half xxs) xxs)) (sort (drop (half xxs) xxs))
    where
        half :: [a] -> Int
        half ys = (length ys) `div` 2
        merge :: Ord a => [a] -> [a] -> [a]
        merge [] [] = []
        merge aas [] = aas
        merge [] bbs = bbs
        merge (a:as) (b:bs) | a < b = a:(merge as (b:bs))
                            | otherwise = b:(merge (a:as) bs)

-- zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ _ [] = []
zipWith _ [] _ = []
zipWith f (a:as) (b:bs) = (f a b):(zipWith f as bs)

-- Show (Mat a) & transpose
data Mat a = Mat {mrows :: [[a]]}
instance Show a => Show (Mat a) where
    show = unlines . map (unwords . map show) . mrows

transpose :: Mat a -> Mat a
transpose (Mat a) = Mat [[a!!x!!y | x <- [0..(n-1)]] | y <- [0..(m-1)]]
    where
        m = length (a!!0)
        n = length a

-- Show (Tri a), trol, tror
data Tri a = Tri {trows :: [[a]]}
instance Show a => Show (Tri a) where
    show (Tri xxs) = unlines [(padding x) ++ (unwords [show y | y <- xxs!!x]) ++ (padding x) | x <- [0..((length xxs) - 1)]]
        where
            padding :: Int -> String
            padding n = [' ' | _ <- [0..((length xxs) - n - 1)]]

trol :: Tri a -> Tri a
trol (Tri xxs) = Tri (rotTri xxs)
    where
        rotTri :: [[a]] -> [[a]]
        rotTri [] = []
        rotTri yys = (rotTri (subTri yys)) ++ [bottomRow yys]
        subTri :: [[a]] -> [[a]]
        subTri yys = tail [tail y | y <- yys]
        bottomRow :: [[a]] -> [a]
        bottomRow [] = []
        bottomRow (x:xs) = (head x):(bottomRow xs)

tror :: Tri a -> Tri a
tror = trol . trol

-- sublists
sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = (sublists xs) ++ map (x:) (sublists xs)

-- prefixes, suffixes
prefixes, suffixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x:xs) = [x]:(map (x:) (prefixes xs))
suffixes [] = []
suffixes xxs = xxs:(suffixes (tail xxs))

-- segments
segments :: [a] -> [[a]]
segments [] = []
segments (x:xs) = ([x]:(map (x:) (segments xs))) ++ (segments xs)

-- parts
parts :: [a] -> [[[a]]]
parts [] = []
parts [x] = [[[x]]]
parts (x:xs) = [p' | p@(ys:etc) <- parts xs, p' <- [[x]:p, (x:ys):etc]]

-- perms
-- skipped for time constraints

-- change
change :: [Int] -> Int -> [[Int]]
change _ 0 = [[]]
change xs v = [c:b | c <- xs, c <= v, b <- change (filter (>= c) xs) (v - c)]

-- ktrain
-- See Ktrain.hs
