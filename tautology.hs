data Prop = Const Bool 
          | Var Char
          | Not Prop 
          | And Prop Prop 
          | Or Prop Prop 
          | Imply Prop Prop 

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

find' :: Eq k => k -> Assoc k v -> v 
find' k t = head [ y | (x,y) <- t , k == x]

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find' x s
eval s (Not x) = not (eval s x)
eval s (And x y) = (eval s x) && (eval s y)
eval s (Or x y) = (eval s x) || (eval s y)
eval s (Imply x y ) = (not (eval s x)) || (eval s y)

vars :: Prop -> [Char]
vars (Const _ ) = []
vars (Var x) = [x]
vars (Not x) = vars x
vars (And x y) = (vars x) ++ vars(y)
vars (Or x y) = (vars x) ++ vars(y)
vars (Imply x y) = (vars x) ++ vars(y)

bools :: Int -> [[Bool]]
bools 0 = []
bools 1 = [[True],[False]]
bools n = [True:xs | xs <- others ] ++ [False:xs | xs <- others ]
        where
            others = bools (n-1)

find'' :: Eq a => a -> [a] -> [a]
find'' x [] = []
find'' x (y:ys) | x == y   = find'' x ys
              | otherwise = y:(find'' x ys)

nodup :: Eq a => [a] -> [a]
nodup [] = []
nodup (x:xs) = x:nodup(find'' x xs)

subst :: Prop -> [Subst]
subst p = map (zip ps) (bools (length ps))
         where 
            ps = nodup (vars p)

istaut :: Prop -> Bool
istaut p = and [eval s p | s <- subst p] 

p1 :: Prop
p1 = And ( Var 'A' ) ( Not ( Var 'A' ) )

p2 :: Prop
p2 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B') 