data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

-- Question 0 (Show function)
instance Show Nat where
    show n = show (nat2int n)

-- Question 1
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult x y = add x (mult x (int2nat (nat2int y - 1)))

-- Question 2
-- data Tree = Leaf Int | Node Tree Int Tree

-- t :: Tree
-- t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- flatten :: Tree -> [Int]
-- flatten (Leaf n) = [n]
-- flatten (Node l n r) = flatten l ++ [n] ++ flatten r

-- occurs :: Int -> Tree -> Bool
-- occurs m (Leaf n) = m == n
-- occurs m (Node l n r) = case compare m n of LT -> occurs m l
--                                             EQ -> True
--                                             GT -> occurs m r

-- Question 3
data Tree = Leaf Int | Node Tree Tree

instance Show Tree where
    show (Leaf n) = show n
    show (Node l r) = concat ["(",show l," ",show r,")"]

balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node ls rs) = abs (leafCount ls - leafCount rs) <= 1 && balanced ls && balanced rs
    where
        leafCount :: Tree -> Int
        leafCount (Leaf _) = 1
        leafCount (Node l r) = leafCount l + leafCount r

-- Question 4
balance :: [Int] -> Tree
balance (x:[]) = Leaf x
balance xs = Node (balance l) (balance r)
        where
            (l, r) = halve xs
            halve :: [Int] -> ([Int],[Int])
            halve xs = (take half xs, reverse (take (len - half) (reverse xs)))
            len = length xs
            half = len `div` 2

-- Questions 5-8
-- Not completed due to other exercises/non-relevance
