grid :: Int -> Int -> [(Int,Int)]
grid x y = [(i,j) | i <- [0..x], j <- [0..y]]

square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- (grid n n), x /= y]

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = [x | y <- [0..(n-1)]]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

factors :: Int -> Int
factors 0 = 0
factors n = sum [x | x <- [1..(n-1)], mod n x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], factors x == x]

