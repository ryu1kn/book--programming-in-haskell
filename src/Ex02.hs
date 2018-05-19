module Ex02 where

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
