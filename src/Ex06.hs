module Ex06 where

-- Q6.1
fac 0 = 1
fac n | n > 0 = n * fac (n-1)

-- Q6.2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- Q6.3
-- (^) :: Int -> Int -> Int
-- m ^ 0 = 1
-- m ^ n = m * m ^ (n-1)

-- Q6.4
euclid :: Int -> Int -> Int
euclid m n | m == n = m
           | m > n  = euclid (m-n) n
           | m < n  = euclid m (n-m)

-- Q6.5
{-
length [1, 2, 3]
= { applying length }
1 + length [2, 3]
= { applying length }
1 + (2 + length [3])
= { applying length }
1 + (2 + (3 + []))
= { applying + }
6

drop 3 [1, 2, 3, 4, 5]
= { applying drop }
drop 2 [2, 3, 4, 5]
= { applying drop }
drop 1 [3, 4, 5]
= { applying drop }
drop 0 [4, 5]
= { applying drop }
[4, 5]

init [1, 2, 3]
= { applying init }
1 : init [2, 3]
= { applying init }
1 : (2 : init [3])
= { applying init }
1 : (2 : [])
= { applying : }
[1, 2]
-}

-- Q6.6
-- and :: [Bool] -> Bool
-- and []     = True
-- and (b:bs) = b && and bs

-- concat :: [[a]] -> [a]
-- concat [] = []
-- concat (xs:xss) = xs ++ concat xss

-- replicate :: Int -> a -> [a]
-- replicate 0 _ = []
-- replicate n x = x : replicate (n-1) x

-- (!!) :: [a] -> Int -> a
-- (x:_) !! 0 = x
-- (_:xs) !! n = xs !! (n-1)

-- elem :: Eq a => a -> [a] -> Bool
-- elem _ [] = False
-- elem y (x:xs) | x == y    = True
--               | otherwise = elem y xs

-- Q6.7
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- Q6.8
msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort first) (msort second)
  where
    (first, second) = halve' xs

halve' :: [a] -> ([a],[a])
halve' [] = ([], [])
halve' xs = (take half xs, drop half xs)
    where
        half = length xs `quot` 2
