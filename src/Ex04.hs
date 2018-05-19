module Ex04 where

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
