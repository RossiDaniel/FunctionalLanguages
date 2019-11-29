data Ciao a = Ciau a | Ciauz (Ciao a) a (Ciao a) deriving Show

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

data Boia a = End a | El a (Boia a) deriving Show

sumtree :: Num a => Tree a -> a
sumtree (Leaf x) = x
sumtree (Node l n r) = (sumtree l) + n + (sumtree r)

tree :: Num a=> Tree a
tree = Node (Leaf 1) 2 (Leaf 2)

ciauz :: Num a=> Ciao [a]
ciauz = Ciauz (Ciau [1,10]) [1,10] (Ciau [1,10])

ueee :: Num a => Ciao ([a] -> a)
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

instance Functor Boia where
    fmap f (End x) = End (f x)
    fmap f (El x y) = El (f x) (fmap f y)

instance Applicative Boia where
    pure x =  End x

    (End x) <*> (End y) = End (x y)
    (End x) <*> (El y ys) =  El (x y) (pure x <*> ys)
    (El x xs) <*> (El y ys) = El (x y) (xs <*> ys)
 
bojon :: Num a => Boia a
bojon = El 5 (El 6 (El 7 (End 8)))

bojon1 :: Num a => Boia a
bojon1 = El 5 (El 6 (El 7 (El 8 (End 9))))

data Test a = Tleaf a | Tnode (Test a) (Test a) deriving Show

instance Functor Test where
    fmap f (Tleaf x) = Tleaf (f x)
    fmap f (Tnode l r) = Tnode (fmap f l) (fmap f r)

instance Applicative Test where
    pure x = Tleaf x

    (Tleaf x) <*> (Tleaf y) = Tleaf (x y)
    (Tleaf x) <*> (Tnode l r) = Tnode (pure x <*> l) (pure x <*> r)
    (Tnode l r) <*> (Tnode l' r') = Tnode (l <*> l') (r <*> r')

 
test1 :: Num a => Test a
test1 = Tnode (Tnode (Tleaf (5)) (Tleaf 5)) (Tnode (Tleaf 5) (Tleaf 5))

