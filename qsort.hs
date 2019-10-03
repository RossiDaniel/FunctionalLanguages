qsort [] = []
qsort (n:ns) = (qsort smaller) ++ [n] ++ (qsort greater)
             where 
                smaller = [x|x <- ns, x <= n]
                greater = [x|x <- ns, x > n]

-- if we change smaller = [x|x <- ns, x <= n] and insert a strictly less in place of less than equal we obtaine an array without repetition