import Data.List 
import Data.Ord

type Peg = String
type Move = (Peg,Peg)

data Report = Bad | Ok deriving Show
type Config =[[Int]]


type State = (Int, Config)
type Info = (Report, [Move])
newtype ST a = S (State -> (a, State))

str2int :: String -> Int 
str2int "a" = 0
str2int "b" = 1
str2int "c" = 2
str2int "d" = 3

app :: ST a -> State -> (a,State)
app (S x) s = x s

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap f st = S (\s -> let (x,s') = app st s in (f x,s'))

instance Applicative ST where
    --pure :: a -> ST a
    pure x = S (\s -> (x,s))

    -- <*> :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = S (\s -> let (f,s') = app stf s
                               (x,s'') = app stx s'
                            in (f x,s''))

instance Monad ST where
    -- >>= :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x,s') = app st s
                          in app (f x) s')

valid :: Move -> State -> Bool
valid (a,b) (_,xs) | length (xs!!(str2int a)) == 0 = False
                   | length (xs!!(str2int b)) == 0 = True
                   | (xs!!(str2int a))!!0 > (xs!!(str2int b))!!0 = False
                   | otherwise = True

performMove :: Move -> State -> State
performMove (a,b) (n,xs) = (n+1,mxs)
                         where
                             x = str2int a
                             y = str2int b
                             disc = head (xs!!x)
                             pegx = tail (xs!!x)
                             pegy = disc: (xs!!y)
                             nxs = take x xs ++ [pegx] ++ drop (x+1) xs
                             mxs = take y nxs ++ [pegy] ++ drop (y+1) nxs



move :: Move -> ST Report   -- ((Report,State))
move m = S(\s -> if valid m s then (Ok,performMove m s) else (Bad,s)) --[[1,2,3,4][][][]]
                            
check1 :: [Move] -> ST Info
check1 [] = return (Ok,[]) --S (\s -> ((ok,[]),s))
check1 (m:ms) = do r <- move m     -- (move m) >>= \r ->
                  case r of
                     Ok -> check1 ms
                     Bad -> return (r,[m])
                
check :: [Move] -> (Int, Config) -> ((Report, [Move]),(Int, Config))
check ms init= app (check1 ms) init

result n= check (hanoi n "a" "b" "c" "d") (0, [[1..n],[],[],[]])


hanoi :: Int -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c d = [ ]
hanoi 1 a b c d = [(a, b)]
hanoi n a b c d = shortest [(hanoi (n - k) a d c b ++ hanoi3 k a b c ++ hanoi (n - k) d b a c) | k <- [1..(n-1)]]

shortest :: [[a]] -> [a]
shortest [] = []
shortest ls = minimumBy (comparing length) ls

hanoi3 :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi3 0 a b c = [ ]
hanoi3 n a b c = hanoi3 (n-1) a c b ++ [(a,b)] ++ hanoi3 (n-1) c b a