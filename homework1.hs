--1)
--toDigits:: Integer -> [Integer]
--toDigitsRev :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (n`div`10) ++ [n`mod`10]

toDigitsRev 0 = []
toDigitsRev n = (n`mod`10):toDigitsRev (n`div`10)
--toDigits 1234 == [1,2,3,4]

--2)

--doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs) = [pre] ++ doubleEveryOther xs
                          where
                            pre = x + x * ((length xs)`mod`2)

--3)
--sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

--4)
--validate :: Integer -> Bool

f 0 = True
f n = False

validate n = f ((sumDigits (doubleEveryOther (toDigits n))) `mod` 10)
    
    
--head (f++t)
--       where
--        f = [False | ((sumDigits (doubleEveryOther (toDigits n))) `mod` 10) > 0]
--      t = [True | ((sumDigits (doubleEveryOther (toDigits n))) `mod` 10) == 0]

