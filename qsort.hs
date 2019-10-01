qsort [] = []
qsort (n:ns) = (qsort smaller) ++ [n] ++ (qsort greater)
             where 
                smaller = [x|x <- ns, x <= n]
                greater = [x|x <- ns, x > n]