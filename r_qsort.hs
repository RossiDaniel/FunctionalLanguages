r_qsort [] = []
r_qsort (x:xs) = r_qsort greater ++ [x] ++ r_qsort smaller
                 where
                    greater = [a | a <- xs, a > x]
                    smaller = [b | b <- xs, b <= x]