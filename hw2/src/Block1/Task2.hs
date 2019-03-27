{-# LANGUAGE InstanceSigs #-}

module Block1.Task2
  (
    Tree(..)
  ) where

import           Control.Applicative (liftA2)

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving Show

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)
  fmap f (Leaf x)     = Leaf (f x)

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  Leaf f     <*> t = fmap f t
  Branch l r <*> t = Branch (l <*> t) (r <*> t)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr acc z (Leaf x)     = acc x z
  foldr acc z (Branch l r) = foldr acc r' l
    where r' = foldr acc z r

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf x)     = Leaf <$> f x
  traverse f (Branch l r) = liftA2 Branch l' r'
    where
      l' = traverse f l
      r' = traverse f r
