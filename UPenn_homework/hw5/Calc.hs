{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM

import qualified Data.Map as M

-- Exercise 1
{-
eval' :: ExprT -> Integer
eval' (Lit m) = m
eval' (Add m n) = (eval' m) + (eval' n)
eval' (Mul m n) = (eval' m) * (eval' n)
-}

eval'' :: ExprT -> Integer
eval'' (ExprT.Lit int) = int
eval'' (ExprT.Add int0 int1) = (+) (eval'' int0) (eval'' int1)
eval'' (ExprT.Mul int0 int1) = (*) (eval'' int0) (eval'' int1)

-- example Ex 2
evalStr :: String ->  Maybe Integer
evalStr str = case parseExp ExprT.Lit ExprT.Add ExprT.Mul str of
    (Just exprT) -> Just (eval'' exprT)
    Nothing -> Nothing


-- example exercise 3
class Expr a where
    mul :: a -> a -> a
    lit :: Integer -> a 
    add :: a -> a -> a 

instance Expr ExprT where
    expr1 `mul` expr2 = ExprT.Mul expr1 expr2
    expr1 `add` expr2 = ExprT.Add expr1 expr2
    lit = ExprT.Lit

instance Expr Integer where
    mul = (*)
    add = (+)
    lit = id

instance Expr Bool where
    mul = (&&)
    add = (||)
    lit = (>0)


-- Exercise 4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    (MinMax int0) `mul` (MinMax int1) = MinMax (if int0 < int1 then int0 else int1)
    (MinMax int0) `add` (MinMax int1) = MinMax (if int0 < int1 then int1 else int0)
    lit = MinMax

instance Expr Mod7 where
    mul (Mod7 x) (Mod7 y) = Mod7 ((x*y) `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ((x+y) `mod` 7)
    lit = Mod7

-- tests in exercise 4
{-
testExp :: Expr a => Maybe a 
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testInteger = parseExp lit add mul "5 + 5 * 5"
testBool = testExp :: Maybe Bool 
testMM = testExp :: Maybe MinMax 
testSat = testExp :: Maybe Mod7 
-}

-- exercise 6
data VarExprT = Mul VarExprT VarExprT
        | Add VarExprT VarExprT
        | Lit Int
        | Var String 

instance HasVars (M.Map String Integer -> Maybe Integer) 


instance Expr (M.Map String Integer -> Maybe Integer)


withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer 

