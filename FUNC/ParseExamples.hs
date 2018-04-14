import Parse
import Data.Char (isAlpha)

-- Name
data Name = Mr String | Ms String
            deriving Show

name :: Parser Name
name = title .*. char ' ' *.. many1 (sat isAlpha)
title :: Parser (String -> Name)
title = Mr ... string "Mr" .|. Ms ... string "Ms"
