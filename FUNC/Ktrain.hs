import System.Environment
import Data.Time.Clock

stringAccuracy :: String -> String -> (Int, Int)
stringAccuracy xxs yys = (length xxs, checkErrors (words xxs) (words yys))
    where
        checkErrors :: [String] -> [String] -> Int
        checkErrors [] xs = length xs
        checkErrors xs [] = length xs
        checkErrors (x:xs) (y:ys) | x == y = checkErrors xs ys
                                  | otherwise = 1 + checkErrors xs ys

testLines :: [String] -> (Int, Int) -> IO (Int, Int)
testLines [] sofar = return sofar
testLines (x:xs) (a,b) = do putStrLn x
                            input <- getLine
                            let (a',b') = stringAccuracy x input
                            putStrLn (unwords [show a', "words,", show b', "errors"])
                            testLines xs (a + a', b + b')

main = do [file] <- getArgs
          text <- readFile file
          let textLines = lines text
          start <- getCurrentTime
          (words, errors) <- testLines textLines (0,0)
          end <- getCurrentTime
        --   let duration = nominalDiffTimeToSeconds (diffUTCTime end start)
          putStrLn (unwords ["accuracy", concat [show (max 0 (words - errors) * 100 `div` words), "%"]])
