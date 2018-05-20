module Ch12 where

import           Data.Char

data Expr = Val Int | Div Expr Expr

evalNotWork :: Expr -> Maybe Int
evalNotWork (Val n)   = Just n
evalNotWork (Div x y) = pure div <*> evalNotWork x <*> evalNotWork y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

evalWork :: Expr -> Maybe Int
evalWork (Val n)   = Just n
evalWork (Div x y) = do n <- evalWork x
                        m <- evalWork y
                        safediv n m
-- evalWork (Div x y) = evalWork x >>= (\n -> evalWork y >>= (\m -> safediv n m))

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = xs >>= (\x -> ys >>= (\y -> return (x,y)))
-- pairs xs ys = do x <- xs
--                  y <- ys
--                  return (x,y)

type State = Int

newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = S (\s -> let (x,s') = app st s
                         in (g x, s'))

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x,s))

    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = S (\s -> let (f,s') = app stf s
                               (x,s'') = app stx s'
                           in (f x, s''))

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x,s') = app st s
                        in app (f x) s')

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
    where (l',n') = rlabel l n
          (r',n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n, n+1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _)   = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do n <- fresh
                     return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')

conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing
