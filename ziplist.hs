newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    --fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z [g x | x <- xs]

instance Applicative ZipList where
    --pure :: a -> ZipList a
    pure x = [x | y<-[0..]]

    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
    Z( gs) <*> Z( xs) = Z [g x | (g,x) <- zip gs xs]
    
