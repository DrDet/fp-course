{-# LANGUAGE InstanceSigs #-}

module Block5.Task3
  (
    Builder(..)
  , fromString
  , toString
  ) where

data Builder = One Char | Many [Builder]
  deriving Show

instance Semigroup Builder where
  (<>) :: Builder -> Builder -> Builder
  Many []    <> x             = x
  x          <> Many []       = x
  x@(One _)  <> Many l        = Many (x:l)
  x          <> y             = Many [x, y]

-- Есть проблема: когда many <> one, получается лишний уровень списка, хотя
-- по факту можно добавить в конец существующего списка всего один символ.
-- Но на обычных списках не умеем за O(1) добавлять в конец.
-- Можно использовать Sequence для быстрой вставки в конец
-- и решения этой проблемы.

instance Monoid Builder where
  mempty :: Builder
  mempty = Many []

fromString :: String -> Builder
fromString = foldMap One

toString :: Builder -> String
toString (One c)  = [c]
toString (Many l) = foldr ((++) . toString) "" l
