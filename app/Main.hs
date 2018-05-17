module Main where

import Lib

main :: IO ()
main = someFunc

-- Q2.1
double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

a = b + c where {b = 1; c = 2}; d = a * 2

-- Q2.3
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- Q2.4 last
myLast1 xs = head (reverse xs)

myLast2 xs = xs !! (length xs - 1)

-- Q2.5 init

myInit1 xs = take (length xs - 1) xs

myInit2 xs = reverse (tail (reverse xs))

-- Q3.2
bools :: [Bool]
bools = [True, False, False]

nums :: [[Int]]
nums = [[1,2,3], [4,5]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x

-- Q4.1
halve :: [a] -> ([a], [a])
halve xs = (take hl xs, drop hl xs)
    where hl = length xs `quot` 2

-- Q4.2
third1 :: [a] -> a
third1 xs = head (tail (tail xs))

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (_:_:x:_) = x

-- Q4.3
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs = []
             | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

-- Q4.4
-- True  || True = True
-- True  || False = True
-- False || True = True
-- False || False = False

-- False || False = False
-- _     || _     = True

-- False || b = b
-- _     || _ = True

-- b || c | b == c    = b
--        | otherwise = True

-- Q4.5
-- (&&) b c =
--     if b == True
--     then if c == True
--          then True
--          else False
--     else False

-- Q4.6
-- (&&) b c =
--     if b == True
--     then c
--     else False

-- Q4.7
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

-- Q4.8
luhnDouble :: Int -> Int
luhnDouble n = if n' > 9 then n' - 9 else n'
    where n' = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn n m l o = sum [luhnDouble n, m, luhnDouble l, o] `div` 10 == 0
