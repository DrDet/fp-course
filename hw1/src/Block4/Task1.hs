{-# LANGUAGE InstanceSigs #-}

module Block4.Task1
  (
    Pair(..)
  , NonEmpty(..)
  ) where

data Pair a = Pair a a
  deriving Show

instance Foldable Pair where
  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr acc z (Pair x y) = acc x (acc y z)

  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair x y) = f x `mappend` f y

data NonEmpty a = a :| [a]
  deriving Show

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr acc z (x :| xs) = acc x (foldr acc z xs)

  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (x :| xs) = f x `mappend` foldMap f xs
