module Golf where

import Data.List

-- Exercise 1
rankList :: [a] -> [Int]
rankList l = [1,2..length(l)]

skips :: [a] -> [[a]]
skips l = map (\ cnt -> [m | (m,i) <- (zip l (rankList l)), i `mod` cnt == 0] ) (rankList l)

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs) 
    | (b > a) && (b > c) = b : localMaxima (b:c:xs)
    | otherwise          = localMaxima (b:c:xs)
localMaxima _ = []

-- Exercise 3
histo:: Int -> (Int, Int) -> String
histo m (i, n) = show i ++ "=" ++ 
            replicate n '*' ++ 
            replicate (m - n) ' '

histogram:: [Integer] -> String
histogram xs = let count = map (\n -> length $ filter (== n) xs) [0..9]
                   m = maximum count in
                unlines $ reverse $ transpose $ map (histo m) $ zip [0..9] count 


