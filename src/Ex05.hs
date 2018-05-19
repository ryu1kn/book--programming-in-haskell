module Ex05 where

-- Q5.1
squareSum :: Int
squareSum = sum [ x^2 | x <- [1..100] ]

-- Q5.2
grid :: Int -> Int -> [(Int,Int)]
grid n m = [(x, y) | x <- [0..n], y <- [0..m]]

-- Q5.3
square :: Int -> [(Int,Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- Q5.4
replicate1 :: Int -> a -> [a]
replicate1 n x = [x | _ <- [1..n]]

-- Q5.5
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- Q5.6
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x]

factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]

-- Q5.7
-- concat [[(x, y) | y <- ys] | x <- xs]

-- Q5.8
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

-- Q5.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
