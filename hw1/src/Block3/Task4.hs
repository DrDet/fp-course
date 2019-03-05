{-# LANGUAGE InstanceSigs #-}

module Block3.Task4
  (
    Tree(..)
  , isEmpty
  , size
  , find
  , insert
  , fromList
  , delete
  ) where

import qualified Data.List     (foldr)

data Tree a = Leaf | Node { vals :: [a], left :: Tree a, right :: Tree a}
  deriving Show

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf            = z
  foldr acc z (Node xs l r) = foldr acc m' l
    where m' = Data.List.foldr acc r' xs
          r' = foldr acc z r

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f = foldr (mappend . f) mempty

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

size :: Tree a -> Int
size Leaf          = 0
size (Node xs l r) = length xs + size l + size r

find :: Ord a => a -> Tree a -> Maybe (Tree a)
find _ Leaf = Nothing
find x tree@(Node xs l r)
  | x `elem` xs = Just tree
  | x < head xs = find x l
  | otherwise   = find x r

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node [x] Leaf Leaf
insert x tree@(Node xs l r)
  | x `elem` xs = tree{ vals = x:xs }
  | x < head xs = tree{ left = insert x l }
  | otherwise   = tree{ right = insert x r }

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf

delete :: Ord a => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x tree@(Node xs l r)
  | x `elem` xs =
    if   length xs > 1
    then tree{ vals = tail xs }
    else merge l r
  | x < head xs = tree{ left = delete x l }
  | otherwise   = tree{ right = delete x r }
  where
    merge :: Tree a -> Tree a -> Tree a
    merge Leaf t                          = t
    merge tree'@Node{ right = rightCh } t = tree'{ right = merge rightCh t }
