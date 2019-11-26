type Peg = String
type Move = (Peg,Peg)

hanoi 1 a b c = [(a,b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (n-1) c b a)

checker xs = foldr (\x y -> ) xs


--hanoi 3 "a" "b" "c"
--hanoi 2 "a" "b" "c"
--hanoi 1 "a" "b" "c"