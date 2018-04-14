module SetSpec where

class SetSpec s where
  empty     :: s a
  singleton :: a -> s a
  add       :: Ord a => a -> s a -> s a
  delete    :: Ord a => a -> s a -> s a
  size      :: s a -> Int
  member    :: Ord a => a -> s a -> Bool
  union     :: Ord a => s a -> s a -> s a
  inter     :: Ord a => s a -> s a -> s a
  diff      :: Ord a => s a -> s a -> s a
  elements  :: s a -> [a]
  showSet   :: Show a => s a -> String
  showSet se = concat (["{"] ++ (intersperse "," (map (show) (elements se))) ++ ["}"])
    where
      intersperse :: String -> [String] -> [String]
      intersperse _ [] = []
      intersperse _ (x:[]) = [x]
      intersperse c (x:xs) = x:c:(intersperse c xs)

set :: (Ord a, SetSpec s) => [a] -> s a
set  =  foldr add empty
