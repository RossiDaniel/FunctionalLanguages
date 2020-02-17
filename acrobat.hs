type Pole = (Int,Int)
data Line a = L (Pole -> (Maybe a ,Pole ))

app :: Line a -> Pole -> (Maybe a,Pole)
app (L l) p = l p

instance Functor Line where
    --fmap :: (a -> b) -> Line a -> Line b
    fmap f l = L(\p -> case app l p of
                          (Nothing, p) -> (Nothing, p)
                          ((Just v),out) -> ((Just (f v)),out))

instance Applicative Line where
    --pure :: a -> Line a
    pure x = L(\p -> (Just x,p))

    -- <*> :: Line (a -> b) -> Line a -> Line b
    pf <*> px = L (\p -> case app pf p of
                            (Nothing,out) -> (Nothing,out) 
                            (Just f,out) -> app (fmap f px) out)

instance Monad Line where
    -- >>= :: Line a -> (a -> Line b) -> Line b
    l >>= f = L(\p -> case app l p of
                         (Nothing,out) -> (Nothing,out)
                         (Just n,out) -> app (f n) out)

checkpol :: Pole -> Bool
checkpol (l,r) | ((max (l-r) (r-l)) < 4) && (l >= 0) && (r >= 0) = True
               | otherwise                                       = False

landLeft :: Int -> Line ()
landLeft n = L(\(l,r)-> if checkpol (l+n,r) then (Just (),(l+n,r)) else (Nothing,(l+n,r)))
landRight :: Int -> Line ()
landRight n = L(\(l,r)-> if checkpol (l,r+n) then (Just (),(l,r+n)) else (Nothing,(l,r+n)))

g :: Line()
g = do landLeft 2
       landRight 4
       landLeft (-1)
       landRight 1