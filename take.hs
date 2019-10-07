take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x:take' (n-1) xs

-- take' 3 [1,2,3,4,5] = [1,2,3]