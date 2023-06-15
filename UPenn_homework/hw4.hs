-- Exercise 1
func1 :: [Integer] -> Integer
func1 = foldr (\ x y -> if even x then (x-2)*y else y) 1

-- fun2 :: Integer -> Integer

-- Exercise 2
data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- foldTree :: [a] -> Tree a
-- foldTree = 


-- Exercise 3
xor :: [Bool] -> Bool
xor l = ( ( length $ filter (\ x -> x == True) l ) `rem` 2 == 1 )

-- map' :: (a -> b) -> [a] -> [b]

-- myFoldl :: (a -> b -> a) -> a -> [b] -> a


-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> x*2+1 ) $ filter  (\ k -> k `notElem` lst )  [1,2..n]
    where lst = [ i+j+2*i*j | i <- [1,2..n], j <- [1,2..n], i+j+2*i*j <= n ]
