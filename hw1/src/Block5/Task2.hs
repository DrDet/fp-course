{-# LANGUAGE InstanceSigs #-}

module Block5.Task2
  (
    NonEmpty(..)
  , ThisOrThat(..)
  , Name(..)
  , Endo(..)
  ) where

data NonEmpty a = a :| [a]

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (x :| []) <> (y :| ys) = x :| (y : ys)
  (x :| xs) <> (y :| ys) = x :| (xs ++ (y : ys))

data ThisOrThat a b = This a | That b | Both a b

instance Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  x@(Both _ _) <> _    = x
  This x       <> That y     = Both x y
  That y       <> This x     = Both x y
  This x       <> Both _ y'  = Both x y'
  That y       <> Both x' _  = Both x' y
  x            <> _          = x

newtype Name = Name String
  deriving Show

instance Semigroup Name where
  (<>) :: Name -> Name -> Name
  Name x <> Name y = Name (x ++ ('.':y))

instance Monoid Name where
  mempty :: Name
  mempty = Name ""

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  x <> y = Endo (f . g)
    where
      f = getEndo x
      g = getEndo y

instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo id
