type Peg = String
type Move = (Peg,Peg)

hanoi3 1 a b c = [(a,b)]
hanoi3 n a b c = (hanoi (n-1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (n-1) c b a)


--hanoi 3 "a" "b" "c"
--hanoi 2 "a" "b" "c"
--hanoi 1 "a" "b" "c"