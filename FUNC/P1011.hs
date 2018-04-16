import Test.FitSpec -- Also imports LeanCheck and LeanCheck.Utils

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

main = mainWith args {names = ["take n xs", "drop n xs"]}
                (take, drop)
                properties
