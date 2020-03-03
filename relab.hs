type State = Int
newtype ST a = S (State -> (a,State))

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

app :: ST a -> State -> (a,State)
app (S st) s = st s 

instance Functor ST where
    --fmap :: (a -> b) -> ST a -> ST b
    fmap g st = S (\s -> let (v,s') = app st s in (g v,s'))

instance Applicative ST where
    --pure :: a -> ST a
    pure x = S(\s -> (x,s))

    -- <*> :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = S(\s -> let (f,s') = app stf s
                          in app (fmap f stx) s')

instance Monad ST where
    -- >>= ST a -> (a -> ST b) -> ST b
    st >>= f = S(\s -> let (x,s') = app st s
                       in app (f x) s')

ritardown :: Tree a -> Tree a -> Tree a
ritardown = (\x y-> Node x y)

fresh :: ST Int
fresh = S (\s -> (s,s+1))

relabel :: Tree a -> ST (Tree Int)
relabel (Leaf _) = Leaf <$> fresh
relabel (Node l r) = Node <$> relabel l <*> relabel r

relabel' :: Tree a -> ST (Tree Int)
relabel' (Leaf _) = fresh >>= (\x -> return (Leaf x))
relabel' (Node l r) = relabel' l >>= (\tl -> (relabel' r)  >>= \tr -> return (Node tl tr))

dorelabel :: Tree a -> ST (Tree Int)
dorelabel (Leaf _) = do x <- fresh
                        return (Leaf x)
dorelabel (Node l r) = do tl <- dorelabel l
                          tr <- dorelabel r
                          return (Node tl tr)

