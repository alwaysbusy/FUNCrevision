import Test.FitSpec -- Also imports LeanCheck and LeanCheck.Utils
import Msort

-- take and drop LeanCheck
prop_length_take :: Int -> [Word2] -> Bool
prop_length_take n xs = min (max n 0) (length xs) == length (take n xs)

prop_length_drop :: Int -> [Word2] -> Bool
prop_length_drop n xs = min ((length xs) - min n (length xs)) (length xs) == length (drop n xs)

prop_takedrop :: Int -> [Word2] -> Bool
prop_takedrop n xs = xs == (take n xs) ++ (drop n xs)

-- take and drop FitSpec
properties :: (Int -> [Word2] -> [Word2], Int -> [Word2] -> [Word2]) -> [Property]
properties (take, drop) = 
    [property $ \n xs -> min (max n 0) (length xs) == length (take n xs)
    ,property $ \n xs -> min ((length xs) - min n (length xs)) (length xs) == length (drop n xs)
    ,property $ \n xs -> xs == (take n xs) ++ (drop n xs)]

-- main = mainWith args {names = ["take n xs", "drop n xs"]}
--                 (take, drop)
--                 properties

-- take and drop Speculate
-- Skipped as not examined

-- msort
checkOrder :: Ord a => [a] -> Bool
checkOrder [] = True
checkOrder (x:[]) = True
checkOrder (x1:x2:xs) = comp /= GT && checkOrder (x2:xs)
    where
        comp = compare x1 x2

prop_order_msort :: [Word2] -> Bool
prop_order_msort ys = checkOrder (msort ys)

prop_length_msort :: [Word2] -> Bool
prop_length_msort xs = length xs == length (msort xs)

prop_order_merge :: [Word2] -> [Word2] -> Bool
prop_order_merge xs ys = checkOrder xs && checkOrder ys ==> checkOrder (merge xs ys)

prop_length_units :: [Word2] -> Bool
prop_length_units xs = length xs == length (units xs)

prop_content_units :: [Word2] -> Bool
prop_content_units [] = True
prop_content_units xxs@(x:xs) = x:[] == head (units xxs) && prop_content_units xs

msort_properties :: ([Word2] -> [[Word2]], [Word2] -> [Word2] -> [Word2], [Word2] -> [Word2]) -> [Property]
msort_properties (units, merge, msort) = 
    [property $ prop_content_units
    ,property $ prop_length_units
    ,property $ prop_order_merge
    ,property $ prop_length_msort
    ,property $ prop_order_msort]

main = mainWith args {names = ["units xs", "merge xs ys", "msort xs"]}
                (units, merge, msort)
                msort_properties
