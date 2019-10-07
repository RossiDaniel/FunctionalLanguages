drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs

-- drop’ 2 [1,2,3,4]= [3,4] 