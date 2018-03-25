-- Question 1
readLine :: IO String
readLine = do
    x <- getChar
    if x == '\n' then
        return []
    else
        do 
            xs <- getLine
            if x == '\DEL' then
                return ("\ESC[1D" ++ xs)
            else
                return (x:xs)

test1 = putStr "Enter a string:" >> readLine >>= putStrLn

-- Question 2
-- See calculator.hs

-- Questions 3-5
-- Not completed due to time constraints

-- Question 6
nimremove :: [Int] -> Int -> Int -> (Bool,[Int])
nimremove board row take
                | board!!(row - 1) >= take && take > 0 = (True,removestars board (row - 1))
                | otherwise = (False,board)
                    where
                        removestars (x:xs) xr = if xr == 0 then (x-take):xs else x:(removestars xs (xr-1))

nimprint :: [Int] -> IO ()
nimprint xxs = printboard xxs 1
                        where
                            printboard :: [Int] -> Int -> IO ()
                            printboard [] _ = putStr ""
                            printboard (x:xs) n = putStrLn (concat [show n, ":", ['*' | y <- [1..x]]]) >> printboard xs (n+1)

nimcheck :: [Int] -> Bool
nimcheck [] = True
nimcheck (x:xs) = x == 0 && nimcheck xs

nimround :: [Int] -> Int -> IO ()
nimround board player = do
    nimprint board
    putStr (concat ["Player ", show (player + 1), "- Choose a row: "])
    rowstr <- getLine
    let row = read rowstr::Int
    if row < 1 || row > length board 
        then do
            putStrLn "Invalid line chosen"
            nimround board player
        else do
            putStr "Choose how many * to remove: "
            take <- getLine
            let (result, newboard) = nimremove board row (read take::Int)
            if result
                then do
                    putStrLn (concat [take, " * removed"])
                    if nimcheck newboard 
                        then putStrLn (concat ["Player ", show (player + 1), " wins!"])
                        else nimround newboard ((player + 1) `mod` 2)
                else do
                    putStrLn "Invalid number removed"
                    nimround board player

playnim = nimround [5,4,3,2,1] 0
