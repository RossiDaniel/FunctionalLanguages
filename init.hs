tail' (x:xs) = xs
reverse' [] = []
reverse' (x:xs) = reverse' (xs) ++ [x]
init' xs = reverse' (tail' (reverse' xs))