import System.Environment
import Prelude hiding (pure)
import Parse
--import Pretty
import Data.Char (isUpper, isAlpha) 
import Data.List (intersperse)

data Decl  =  Decl Name [Cons]
              deriving Show

data Cons  =  Cons Name [Arg]
              deriving Show

data Arg   =  VarA | AppA Name
              deriving Show

type Name  =  String

-- These parser components may be useful.

newLine :: Parser Char
newLine  =  char '\n'

space :: Parser Char
space  =  char ' '

indent :: Parser ()
indent  =  () ... many space ..* many (newLine ..* many1 space)  

symbol  :: String -> Parser String
symbol s  =  string s ..* indent

decl :: Parser Decl
decl  =  Decl .:. sym "data" *.. name ..* var .*. sym "=" *.. many1 (cons .|. sym "|" *.. cons)

cons :: Parser Cons
cons = Cons .:. name .*. (many arg)

arg :: Parser Arg
arg = VarA ... var .|. AppA .:. sym "(" *.. name ..* var ..* sym ")" ..* indent

name :: Parser Name
name  = many1 (sat isAlpha) `satisfying` (isUpper . head) ..* indent -- Also needs to handle lower case letters

var :: Parser ()
var = () ... sym "t" ..* indent

--declsDoc :: [Decl] -> DOC
--declsDoc  =  error "Declare a working 'declsDoc'. See Q2(b)."

--prettyDecls :: Int -> [Decl] -> String
--prettyDecls w ds  =  pretty w (declsDoc ds)

class Size c where
  size :: c t -> Int

data List t  =  Nil | Snoc (List t) t

instance Size List where
  size Nil          =  0
  size (Snoc xs _)  =  size xs + 1
  
instance Size Maybe where
  size Nothing = 0
  size (Just _) = 1

data CatTree t  =  One t | Cat (CatTree t) (CatTree t)

instance Size CatTree where
    size (One _) = 1
    size (Cat x y) = size x + size y

-- Add Size instances for Maybe and CatTree.  See Q2(c).

sizeInstance :: Decl -> String
sizeInstance (Decl n vs) = unlines ((unwords ["instance Size", n, "where"]):[unwords ["  size", makeName x args, "=", if length args == 0 then "0" else (unwords (intersperse "+" (sizeArgs args)))] | Cons x args <- vs]) --, argV <- unwords (intersperse '+' [sizeAg ag | ag <- args])])
    where
        alphas :: String
        alphas = ['a'..'z']
        makeName :: Name -> [Arg] -> String
        makeName m ags = "(" ++ (unwords (m:["a" | a <- ags])) ++ ")"
        sizeArgs :: [Arg] -> [String]
        sizeArgs [] = []
        sizeArgs (x:xs) = (sizeAg x):(sizeArgs xs)
        sizeAg :: Arg -> String
        sizeAg VarA = "1"
        sizeAg (AppA m) = "size m"

main  =  do src <- getContents
            let Just ds  =  parseWith (many decl) src
            [w] <- getArgs
            --putStrLn (prettyDecls (read w) ds)
            mapM_ (putStr . sizeInstance) ds

