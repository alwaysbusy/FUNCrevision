module P456 where
import P123 hiding (take, drop, zipWith)
import Parse
import Prelude hiding (exp, pure)

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

-- Ha!
data Prog = Prog [Eqn]
            deriving Show
data Eqn = Eqn Name [Pat] Exp
            deriving Show
data Exp = Nil | Var Name | App Name [Exp] | Cons Exp Exp
            deriving Show
data Pat = PNil | PVar Name | PCons Name Name
            deriving Show
type Name = String


prog :: Parser Prog
prog = Prog .:. many1 eqn

eqn :: Parser Eqn
eqn = Eqn .:. name .*. many pat ..* sym "=" .*. exp

exp :: Parser Exp
exp = consOrArg .:. app .*. (Just .:. sym ":" *.. exp .|. pure Nothing)
    where
        consOrArg a (Just ex) = Cons a ex
        consOrArg a Nothing = a

app :: Parser Exp
app = appOrVar .:. name .*. many arg
    where
        appOrVar n [] = Var n
        appOrVar n as = App n as

arg :: Parser Exp
arg = Nil ... sym "[]"
        .|. Var .:. name
        .|. sym "(" *.. exp ..* sym ")"

pat :: Parser Pat
pat = PNil ... sym "[]"
        .|. PVar .:. name
        .|. PCons .:. sym "(" *.. name ..* sym ":" .*. name ..* sym ")"

name :: Parser String
name = many1 lower
