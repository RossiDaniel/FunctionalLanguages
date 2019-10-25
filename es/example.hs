
(foldr' f v) [] = v
foldr' f v (x:xs) =  f x ((foldr' f v) xs)

sum' xs = foldr (+) 0 xs
--all' :: (a->Bool) -> [Bool] -> Bool
--all' f (x:xs) = 

--any' :: (a-> Bool) -> [Bool] -> Bool
--any' f [] = False
--any' f (x:xs) = (f x) or (any' f xs) 

--all' :: (a -> Bool) -> [a] -> Bool
--all' f (x:xs) = []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x       = x:takeWhile' f xs
                    | otherwise = takeWhile' f xs


