import Data.Char (isDigit)
import Data.List (sort, transpose, permutations, isInfixOf, intersperse)
import System.IO (hFlush, stdout)
import Test.FitSpec hiding (rows)
import System.Random (randomRIO, Random)

-- This program is about sliding-block puzzles in a
-- a 3x3 grid with eight tiles and one free space.
-- If the tiles are assigned digits 1 to 8 then the
-- goal is to reach
-- +---+---+---+
-- | 1 | 2 | 3 |
-- +---+---+---+
-- | 4 | 5 | 6 |
-- +---+---+---+
-- | 7 | 8 |   |
-- +---+---+---+
-- from some initial configuration of tiles. 

data Eight  =  E String

-- data invariant for Eight values; the second
-- conjunct ensures a solvable configuration
invariant :: Eight -> Bool
invariant (E s)  =  sort s == " 12345678" &&
                    even (falls (filter isDigit s))

isGoal :: Eight -> Bool
isGoal (E s)  =  s == "12345678 "

falls :: Ord a => [a] -> Int
falls (x:[]) = 0
falls (x1:x2:xs) = if comp x1 x2 == GT then
                        1 + (falls (x2:xs))
                   else
                        falls (x2:xs)
    where
        comp :: Ord a => a -> a -> Ordering
        comp x y = compare x y

rows, cols :: Eight -> [String]
rows (E s)  =  chop 3 s
cols        =  transpose . rows

chop :: Int -> [a] -> [[a]]
chop _ []  =  []
chop n xs  =  take n xs : chop n (drop n xs)

unrows, uncols :: [String] -> Eight
unrows  =  E . concat
uncols  =  unrows . transpose

instance Show Eight where
  show ei = unlines ((line:(intersperse line (map (makeRow) (rows ei)))) ++ [line])
    where
        sep :: String
        sep = "|"
        line :: String
        line = concat ("+":["---+" | x <- [0..2]])
        makeRow :: [Char] -> String
        makeRow [] = "|"
        makeRow (x:xs) = concat ["| ",[x]," "] ++ (makeRow xs)

validMove :: String -> Eight -> Bool
validMove [t] e  =  or [adjacent t ' ' r | r <- rows e] ||
                    or [adjacent t ' ' c | c <- cols e]
validMove _   _  =  False

-- assumes each item occurs at most once
adjacent :: Eq a => a -> a -> [a] -> Bool
adjacent a b (x:y:etc)  |  x == a
                        =  y == b
                        |  x == b
                        =  y == a
                        |  otherwise
                        =  adjacent a b (y:etc)
adjacent _ _ _          =  False                


-- precondition: validMove [t] e
makeMove :: Char -> Eight -> Eight
makeMove t e  |  or [adjacent t ' ' r | r <- rows e]
              =  unrows [swapIfAdjacent t ' ' r | r <- rows e]
              |  or [adjacent t ' ' c | c <- cols e]
              =  uncols [swapIfAdjacent t ' ' c | c <- cols e]
              |  otherwise
              =  error "makeMove: not a valid move"

swapIfAdjacent :: Eq a => a -> a -> [a] -> [a]
swapIfAdjacent _ _ [] = []
swapIfAdjacent _ _ (x:[]) = [x]
swapIfAdjacent a b (x1:x2:xs) | (a == x1 && b == x2) || (a == x2 && b == x1) = x2:x1:xs
                              | otherwise = x1:(swapIfAdjacent a b (x2:xs))
           
properties :: (Word2 -> Word2 -> [Word2] -> [Word2]) -> [Property]
properties swapIfAdjacent = 
    [property $ \a b xs -> (a `elem` xs && b `elem` xs) == False ==> xs == swapIfAdjacent a b xs
    ,property $ \a b xs -> firstAdjacent a b xs /= [] ==> isInfixOf (reverse (firstAdjacent a b xs)) (swapIfAdjacent a b xs)
    ,property $ \a b xs -> length xs == length (swapIfAdjacent a b xs)]
    
firstAdjacent :: Eq a => a -> a -> [a] -> [a]
firstAdjacent _ _ [] = []
firstAdjacent _ _ (x:[]) = []
firstAdjacent a b (x1:x2:xs) | (a == x1 && b == x2) || (a == x2 && b == x1) = x1:[x2]
                             | otherwise = firstAdjacent a b (x2:xs)
    
main :: IO ()
main = mainWith args {names = ["swapIfAdjacent a b xs"]}
                swapIfAdjacent
                properties

interSolveRandom :: IO ()
interSolveRandom  =  do e <- randomEight
                        interSolve e
                        interSolveRandom

interSolve :: Eight -> IO ()
interSolve e    =  do putStr (show e)
                      if isGoal e then putStrLn "Goal!"
                      else do putStrLn $ "Goal is "++show (minSolve e)++" moves away."
                              t <- getValidMove e
                              interSolve (makeMove t e)

getValidMove :: Eight -> IO Char
getValidMove e  =  do putStr "Move? " ; hFlush stdout
                      input <- getLine
                      if validMove input e then return (Prelude.head input)
                      else do putStrLn "Not valid."
                              getValidMove e

randomEight :: IO Eight
randomEight  =  do rand <- randomRIO (0, (length allEights - 1))
                   return (E (allEights!!rand))
    where
        allEights :: [String]
        allEights = filter (even . falls . filter isDigit) (permutations " 12345678")

data State = State Eight Int
        
minSolve :: Eight -> Int
minSolve st = findNext (State st 0) []
    where
        findNext :: State -> [String] -> Int
        findNext (State e count) _ | isGoal e = count
        findNext (State e count) history = if length nextMoves == 0 then 100000 else minimum nextMoves 
            where
                nextMoves = [findNext (State (makeMove poss e) (count + 1)) ((estr e):history) | [poss] <- map show [1..8], validMove [poss] e, (estr (makeMove poss e) `elem` history) == False]
                estr :: Eight -> String
                estr (E s) = s

