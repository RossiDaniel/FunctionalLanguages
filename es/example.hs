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


data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = (eval x) `div` (eval y)

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

eval' :: Expr -> Maybe Int 
eval' (Val n) = Just n
eval' (Div x y) = case eval' x of 
                     Nothing -> Nothing
                     Just n -> case eval' y of 
                                  Nothing -> Nothing
                                  Just m -> safediv n m

--eval'' :: Expr -> Maybe Int
--eval'' (Val n) = pure n
--eval'' (Div x y) = pure safediv <*> eval'' x <*> eval'' y
--non funziona perché safediv è di tipo Int -> Int -> Maybe Int mentre la funzione che vogliamo noi deve essere del tipo Int -> Int -> Int così che poi venga inserita dentro la struttura dati Maybe come segue Maybe (Int -> Int -> Int)

eval'' :: Expr -> Maybe Int
eval'' (Val n) = Just n
eval'' (Div x y) = eval'' x >>= \n ->
                   eval'' y >>= \m -> 
                   safediv n m

evall :: Expr -> Maybe Int 
evall (Val n) = Just n
evall (Div x y) = do n <- evall x
                     m <- evall y
                     safediv n m

data Alb a = Foia | Nodo (Alb a) a (Alb a) deriving Show

instance Functor Alb where
    fmap f (Foia) = Foia
    fmap f (Nodo l n r) = Nodo (fmap f l) (f n) (fmap f r)
instance Applicative Alb where
    pure x = 
-- (>>=) :: Alb a -> (a -> Alb b) -> Alb b
instance Monad Alb where 
    (Foia) >>= _ = Foia
    (Nodo l n r) >>= f = f n