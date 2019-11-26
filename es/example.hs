data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat

add Zero n = n
add (Succ m) n = Succ (add m n)

molt :: Nat -> Nat -> Nat
molt Zero n = Zero
molt (Succ m) n = add (molt m n) n

data Tree a = Leaf a | Node (Tree a) (Tree a)

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs  = Node (balance l1) (balance l2)
            where
             l1 = [x | (x,y) <- (zip xs [0..(length xs)]) , even y]
             l2 = [x | (x,y) <- (zip xs [0..(length xs)]) , odd  y]


map' :: (a->b) -> [a] -> [b]

map' f xs = foldr (\y ys -> (f y):ys) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs  = foldr (\y ys -> [y | f y] ++ ys) [] xs
--(v # (f x # ()))

all' :: (a -> Bool) -> [a] -> Bool
all' f xs = foldr (\y r -> (f y) && r) True xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

dec2int :: [Int] -> Int
dec2int xs = foldl (\ x y -> x*10+y ) 0 xs
