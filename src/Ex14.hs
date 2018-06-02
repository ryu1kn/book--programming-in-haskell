module Ex14 where

import           Data.Foldable
import           Data.Traversable

-- -- Q14.1
{-
instance (Monoid a, Monoid b) => Monoid (a,b) where
    -- mempty :: (a,b)
    mempty = (mempty,mempty)

    -- mappend :: (a,b) -> (a,b) -> (a,b)
    (x1,y1) `mappend` (x2,y2) = (x1 `mappend` x2, y1 `mappend` y2)
-}

-- Q14.2
{-
instance Monoid b => Monoid (a -> b) where
    -- mempty :: a -> b
    mempty = \_ -> mempty

    -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
    f `mappend` g = \x -> f x `mappend` g x
-}

-- Q14.3
{-
instance Foldable Maybe where
    -- fold :: Monoid a => Maybe a -> a
    fold (Just x) = x
    fold Nothing  = mempty

    -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
    foldMap f (Just x) = f x
    foldMap f Nothing  = mempty

    -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
    foldr f y (Just x) = f x y
    foldr f y Nothing  = mempty

    -- foldl :: (a -> b -> a) -> a -> Maybe b -> a
    foldl g x (Just y) = g x y
    foldl g x Nothing  = mempty

instance Traversable Maybe where
    traverse f (Just x) = fmap Just (f x)
    traverse f Nothing  = Nothing
-}

-- Q14.4
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Foldable Tree where
    -- fold :: Monoid a => Tree a -> a
    fold Leaf         = mempty
    fold (Node l x r) = (fold l) `mappend` x `mappend` (fold r)

    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap f Leaf         = mempty
    foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f v Leaf         = v
    foldr f v (Node l x r) = foldr f (f x (foldr f v r)) l

    -- foldl :: (a -> b -> a) -> a -> Tree b -> a
    foldl f v Leaf         = v
    foldl f v (Node l x r) = foldl f (f (foldl f v l) x) r
