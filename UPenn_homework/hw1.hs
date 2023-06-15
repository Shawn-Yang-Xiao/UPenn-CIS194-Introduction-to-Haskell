-- Ex1: toDigits
toDigits :: Integer -> [Integer]
toDigits a
    | a <= 0    = []
    | otherwise = (toDigits (a `div` 10) ) ++ [ a `mod` 10 ]

toDigitsRev :: Integer -> [Integer]
toDigitsRev a
    | a <= 0    = []
    | otherwise = (a `mod` 10) : toDigitsRev (a `div` 10)

-- Ex2: double every other digit

doubleFromLeft :: [Integer] -> [Integer]
doubleFromLeft a
    | a == []       = []
    | tail(a) == [] = [2 * head(a)]
    | otherwise     = [head(a) * 2] ++ [head(tail(a))] ++ doubleFromLeft (tail(tail(a))) 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther a
    | a == []                   = []
    | length(a) `mod` 2 == 0    = (2*head(a)) : doubleEveryOther (tail(a))
    | otherwise                 = (head(a)) : doubleEveryOther (tail(a))

-- Ex3: sum digits
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs


-- Ex4: validate credit card number
validate :: Integer -> Bool
validate a 
    | (sumDigits ( doubleEveryOther (toDigits a) ) ) `mod` 10 == 8  = True
    | otherwise         = False


-- Ex5: the towers of hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi m a b c
    | m == 1    = [(a,b)]
    | otherwise = [hanoi (m-1) ++ ++ ]


