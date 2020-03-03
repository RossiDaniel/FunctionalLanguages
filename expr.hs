data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr a where
    --fmap :: (a -> b) -> Expr a -> Expr b
    fmap f (Var x) = Var (f x)
    fmap f (Val x) = Val x
    fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
    --pure :: a -> Expr a
    pure x = Var x

    -- <*> :: Expr (a -> b) -> Expr a -> Expr b
    (Var f) <*> px = fmap f px
    (Add lf rf) <*> px = Add (lf <*> px) (rf <*> px) 
 
instance Monad Expr where
    -- >>= Expr a -> (a -> Expr b) -> Expr b
    (Var x) >>= f = f x
    (Val x) >>= _ = Val x
    (Add l r) >>= f = Add (l >>= f) (r >>= f)s
