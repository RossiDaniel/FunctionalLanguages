x\type State = Int 

newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
    --fmap :: (a -> b) -> ST a -> ST b
    fmap g st = S (\s -> let(x,s') = app st s in (g x, s'))

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l1 r1, n2)
                    where
                        (l1,n1) = rlabel l n
                        (r1,n2)= rlabel r n1 

