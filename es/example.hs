data Move = North | South | East | West
            deriving Show

type Pos = (Int , Int)
        
move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x, y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

mov :: [Move]
mov = [West,North,East,East,East,East,East,East,East,North]

moves [] p = p
moves (x:xs) p = moves xs (move x p)

moves' :: [Move] -> Pos -> Pos
moves' xs p = foldl (\a b -> move b a) p xs

sum' p xs = foldr' (+) 0 xs

foldr' p v [] = v
foldr' p v (x:xs) = p x (foldr' p v xs) 

foldl' p v [] = v
foldl' p v (x:xs) = foldl' p (p v x) xs

moves2 :: [Move] -> Pos -> Pos
moves2 = foldr (\ a b -> (\x -> b (move a x) )) id

ciao = (\ a b -> (\x -> b (move a x) ))

data Shape = Circle Float | Rect Float Float
data Ciao = Hello | Ciaone 

square :: Float -> Shape
square n = Rect n n