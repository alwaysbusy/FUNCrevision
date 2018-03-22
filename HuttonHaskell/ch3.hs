-- Question 1
a :: [Char]
a = ['a', 'b', 'c']

b :: (Char,Char,Char)
b = ('a', 'b', 'c')

c :: [(Bool,Char)]
c = [(False, 'O'), (True, '1')]

d :: ([Bool],[Char])
d = ([False, True], ['0', '1'])

e :: [[Int] -> [Int]]
e = [tail, init, reverse]

-- Question 2
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Int -> Int
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)
