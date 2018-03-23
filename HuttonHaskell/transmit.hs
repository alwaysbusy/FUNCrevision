-- Binary string transmitter example from chapter 7 of Programming
-- in Haskell, Graham Hutton, Cambridge University Press, 2016.
-- Modified to include parity checking, Owen Hurford

import Data.Char

-- Base conversion

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- Transmission

encode :: String -> [Bit]
encode = concat . map (addparity . make8 . int2bin . ord)

parity :: [Bit] -> Bit -- Added
parity xs = length (filter (==1) xs) `mod` 2 -- Added

addparity :: [Bit] -> [Bit] -- Added
addparity xs = xs ++ [parity xs] -- Added

chop9 :: [Bit] -> [[Bit]] -- Modified
chop9 []   = [] -- Modified
chop9 bits = take 9 bits : chop9 (drop 9 bits) -- Modified

testparity :: [Bit] -> Bool -- Added
testparity xs = parity (take 8 xs) == last xs -- Added

dropparity :: [Bit] -> [Bit] -- Added
dropparity xs -- Added
    | testparity xs = take 8 xs -- Added
    | otherwise = error("Parity invalid") -- Added

decode :: [Bit] -> String
decode = map (chr . bin2int . dropparity) . chop9 -- Modified

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- Question 9
forgetchannel :: [Bit] -> [Bit]
forgetchannel = tail

transfer :: String -> String
transfer = decode . forgetchannel . encode
