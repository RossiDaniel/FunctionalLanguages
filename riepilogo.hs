{--
type State = Int
newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) s = st s

instance Functor ST where
    --fmap :: (a -> b) -> ST a -> ST b
    fmap f st = S (\s -> let (x,s') = app st s in (f x,s'))

instance Applicative ST where 
    --pure :: a -> ST a
    pure x = S (\s -> (x,s))

    -- <*> :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = S (\s -> let (f,s') = app stf s in app (fmap f stx) s')

instance Monad ST where
    -- >>= ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

fresh :: ST Int
fresh = S(\s -> (s,s+1))

rlabel (Leaf _) = Leaf <$> fresh
rlabel (Node l r) = Node <$> rlabel l <*> rlabel r

t1 :: Tree Bool
t1 = (Node (Node (Leaf True) (Leaf True)) (Leaf True))
--}


{--
type State = String
newtype Pila a = P (State -> [(a,State)])

app :: Pila a -> State -> [(a,State)]
app (P p) s = p s

instance Functor Pila where
    --fmap :: (a -> b) -> Pila a -> Pila b
    fmap f p = P (\s -> case app p s of
                             [] -> []
                             [(v,out)] -> [(f v,out)])

instance Applicative Pila where
    --pure :: a -> Pila a
    pure x = P(\s -> [(x,s)])

    -- <*> :: P (a -> b) -> Pila a -> Pila b
    pf <*> px = P(\s -> case app pf s of 
                           [] -> []
                           [(f,out)] -> app (fmap f px) out)

instance Monad Pila where
    -- >>= :: Pila a -> (a -> Pila b) -> Pila b
    p >>= f = P(\s -> case app p s of
                         [] -> []
                         [(x,out)] -> app (f x) out) 


pop :: Pila Char
pop = P(\xs -> case xs of
                  [] -> []
                  (x:xs) -> [(x,xs)])

push :: Char -> Pila ()
push c = P(\s -> [((),c:s)])

balance :: String -> Pila Bool
balance [] = P(\s -> case s of
                          [] -> [(True,[])]
                          xs -> [(False,xs)])
balance ('(':xs) = do push '('
                      balance xs

balance (')':xs) = do pop
                      balance xs

balance (x:xs) = balance xs 


--}


instance Functor ((->) a) where
    --fmap :: (b -> c) (a -> b) (a -> c)
    fmap f g = \x -> f (g x)

instance Applicative ((->) a) where
    --pure :: a -> ((->) a)
    pure x = (\z y -> z) x

    -- <*> :: (a -> (b -> c) -> (a -> b) -> (a -> c))
    f <*> g = \x -> (f x) (g x)

instance Monad ((->) a) where
    -- >>= (a -> b) -> (b -> (a -> c)) -> (a -> c)
    f >>= g = \x -> g (f x) x