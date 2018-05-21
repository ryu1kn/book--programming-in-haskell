module Ex07 where

import           Data.Foldable

-- Q7.1
-- [f x | x <- xs, p x]
-- map f (filter p xs)

-- Q7.2
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) | p x       = x : Ex07.takeWhile p xs
                   | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs) | p x       = Ex07.dropWhile p xs
                   | otherwise = x:xs

-- Q7.3
-- map f = foldr (\x xs -> f x : xs) []
-- filter p = foldr (\x xs -> if p x then x:xs else xs) []

-- Q7.4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

-- Q7.5
curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x y -> f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = \(x, y) -> f x y

-- Q7.6
