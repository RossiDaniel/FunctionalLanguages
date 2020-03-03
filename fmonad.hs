type State = Int
newtype ST a = S (State -> (a,State))

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

app :: ST a -> State -> (a,State)
app (S st) s = st s 


instance Monad ST where
    -- >>= ST a -> (a -> ST b) -> ST b
    st >>= f = S(\s -> let (x,s') = app st s
                       in app (f x) s')

instance Functor ST where
    --fmap :: (a -> b) -> ST a -> ST b
    fmap f st = do x <- st
                   return (f x)

instance Applicative ST where
    --pure :: a -> ST a
    pure x = S(\s -> (x,s))

    -- <*> :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do f <- stf
                     fmap f stx