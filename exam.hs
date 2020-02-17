{--

all :: (a -> Bool) -> [a] -> [Bool]
all _ [] = True
all f (x:xs) = (f x) && (all f xs)

all f xs = foldr (\x y -> (f x) && y ) True xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf x) = Leaf
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)
instance Functor ((->) a) of
    -- fmap :: (b -> c) -> (a -> b) (a -> c)
    fmap f g = \a -> f (g a)(

instance Applicative ((->) a) where
    --pure :: b -> (a -> b)
    pure b = \a -> b

    -- <*> :: (a -> (b -> c)) -> (a -> b) -> (a -> c)
    pf <*> px = \a -> pf a (px a)


instance Monad ((->) a) where
    -- >>= (a -> b) -> (b -> (a-> c)) -> (a -> c)
    g >> f = \a -> f (g a) a 
--}
{--
fib :: [Integer]
fib = [0,1] ++ (fibs [0,1])

fibs :: [Integer] -> [Integer]
fibs xs = (sum xs) : (fibs (tail (xs ++ [(sum xs)])))

fib' :: [Integer]
fib' = 0 : (fibs' [0,1])

fibs' :: [Integer] -> [Integer]
fibs' xs = (tail xs) ++ (fibs' ((tail xs) ++ [y+z | (y,z) <- zip xs (tail xs)]))--}

fibs = 0:1:[x+y | (x,y) <- zip fibs (tail fibs)]


