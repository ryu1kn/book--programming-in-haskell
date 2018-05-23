module Ex12 where

-- Q12.1
data Tree a = Leaf a | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x)     = Leaf (g x)
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

-- Q12.2
-- instance Functor ((->) a) where
--     -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
--     fmap = (.)

-- Q12.3
-- instance Applicative ((->) a) where
--     -- pure :: a -> (b -> a)
--     pure = const

--     -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
--     g <*> h = \x -> g x (h x)

-- Q12.4
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z [x]

    -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]

-- Q12.5
{-
-- f (a -> a) <*> f a -> f a
pure id <*> x = x

-- f ((a -> b) $ a) = f (a -> b) <*> f a
pure (g x) = pure g <*> pure x

-- f (a -> b) <*> f a = f ((->) y) <*> f (a -> b)
x <*> pure y = pure (\g -> g y) <*> x
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-}

-- Q12.6
-- instance Monad ((->) a) where
--     -- (>>=) :: f a -> (a -> f b) -> f b
--     -- (>>=) :: (a -> b) -> (a -> b -> c) -> (b -> c)
--     g >>= h = h . g

-- Q12.7
-- data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
--               deriving Show

-- instance Functor Expr where
--     -- fmap :: (a -> b) -> Expr a -> Expr b
--     fmap g (Var x)   = Var (g x)
--     fmap _ (Val x)   = Val x
--     fmap g (Add x y) = Add (fmap g x) (fmap g y)

-- instance Applicative Expr where
--     -- pure :: a -> Expr a
--     pure = Var

--     -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
--     Add <*> x = Add
