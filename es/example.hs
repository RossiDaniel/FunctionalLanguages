data Ciao a = Ciau a | Ciauz (Ciao a) a (Ciao a) deriving Show

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

sumtree :: Num a => Tree a -> a
sumtree (Leaf x) = x
sumtree (Node l n r) = (sumtree l) + n + (sumtree r)

tree :: Num a=> Tree a
tree = Node (Leaf 1) 2 (Leaf 2)

ciauz :: Num a=> Ciao [a]
ciauz = Ciauz (Ciau [1,10]) [1,10] (Ciau [1,10])

ueee :: Num a => Ciao ([a] -> b)
ueee = Ciauz (Ciau (\x -> sum x )) (\x -> sum x) (Ciau (\x -> sum x))


instance Functor Ciao where
    fmap f (Ciau x) = Ciau (f x)
    fmap f (Ciauz l n r) = Ciauz (fmap f l) (f n) (fmap f r)

class Functor1 f where
    fmap0 :: (a -> b -> c) -> f a -> f b -> f c 

instance Functor1 Ciao where
    fmap0 f (Ciau x) (Ciau y) = Ciau (f x y)
    fmap0 f (Ciauz l n r) (Ciauz ll nn rr) = Ciauz (fmap0 f l ll) (f n nn) (fmap0 f r rr)

sbin :: [Bool] -> [Bool]
sbin [] = [True]
sbin (x:xs) | x == False = True:xs
            | otherwise  = False:(sbin xs)

int2bin :: Int  -> [Bool]
int2bin 0 = [False]
int2bin n = sbin ( int2bin (n-1) )

ciauzzone :: Int -> Int -> [Bool]
ciauzzone x y = int2bin (x+y)

instance Applicative Ciao where
    pure x= (Ciau x)
    
    (Ciau f) <*> (Ciau x) = pure (f x)
    (Ciauz fl f fr) <*> (Ciauz l n r) = Ciauz (fl <*> l) (f n) (fr <*> r)