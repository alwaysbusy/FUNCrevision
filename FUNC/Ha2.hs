import Prelude hiding (pure, exp)
import Data.Char (isAlpha)
import Parse

data Prog = Prog [Eqn]
    deriving (Show)
data Eqn = Eqn Name [Pat] Exp
    deriving (Show)
data Exp = Nil | Var Name | App Name [Exp] | Cons Exp Exp
    deriving (Show)
data Pat = PNil | PVar Name | PCons Name Name
    deriving (Show)
type Name = String

prog :: Parser Prog
prog = Prog .:. many1 (eqn)

eqn :: Parser Eqn
eqn = Eqn .:. name .*. many(pat) ..* sym "=" .*. exp ..* many newLine

exp :: Parser Exp
exp = app .|. app ..* sym ":" *.. exp
    where

app :: Parser Exp
app = App .:. name .*. many(arg)

arg :: Parser Exp
arg = Nil ... sym "[]" .|. Var .:. name .|. sym "(" *.. exp ..* sym ")"

pat :: Parser Pat
pat = PNil ... sym "[]" .|. PVar .:. name .|. PCons .:. sym "(" *.. name ..* sym ":" .*. name ..* sym ")"

name :: Parser Name
name = many1 (sat isAlpha)

newLine :: Parser Char
newLine  =  char '\n'
