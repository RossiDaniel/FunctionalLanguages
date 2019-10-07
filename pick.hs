pick (x:xs) 0 = x
pick (x:xs) n = pick xs n-1

-- pick [1,2,3,4] 2 = 3