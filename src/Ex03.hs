module Ex03 where

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
