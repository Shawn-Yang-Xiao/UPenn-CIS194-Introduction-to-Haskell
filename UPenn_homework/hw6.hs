-- Upenn haskell homework 6 
-- Fibonacci.hs
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
import Control.Lens
-- method Cons, package cannot been found

-- Ex 1 
fibArray :: Integer -> [Integer] -> Integer
fibArray 0 xs = 0
fibArray 1 xs = head(xs)
fibArray n (a:(b:xs)) = fibArray (n-1) ([a+b]++[a]++[b]++xs) 

fibs1 :: Integer -> Integer
fibs1 n = fibArray n [1,0]


-- Ex 2
fibs2 :: [Integer]
fibs2 = fibgen 0 1 where
    fibgen a b = a : fibgen b (a+b)


-- Ex 3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show = loop (20::Int) where 
        loop 0 _ = "\n" 
        loop n (Cons x0 rest) = show x0 ++ ", " ++ loop (n-1) rest

streamToList :: Stream a -> [a]
streamToList :: (Cons x0 rest) = x0 : streamToList rest



