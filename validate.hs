module Validate where 

--1)
toDigits:: Integer -> [Integer]
toDigits n | n <= 0     = []
           | otherwise  = toDigits (n`div`10) ++ [n`mod`10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

--2)
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = [x + x*(y`mod`2) | (x,y) <- (reverse (zip (reverse xs) [0..(sum [1 | x <- xs])]))]

--3)
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

--4)
validate :: Integer -> Bool
validate n = ((sumDigits (doubleEveryOther (toDigits n))) `mod` 10) == 0