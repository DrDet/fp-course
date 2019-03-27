{-# LANGUAGE InstanceSigs #-}

module Block3.Task1
  (
    Parser(..)
  ) where

import           Control.Applicative (Alternative, empty, (<|>))

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser runP) = Parser $ fmap f' . runP
    where
      f' (x, l) = (f x, l)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure x = Parser $ \l -> Just (x, l)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  Parser runPf <*> Parser runPx = Parser $ \l -> case runPf l of
    Nothing       -> Nothing
    Just (f, l')  -> case runPx l' of
      Nothing       -> Nothing
      Just (x, l'') -> Just (f x, l'')

instance Monad (Parser s) where
  return :: a -> Parser s a
  return = pure

  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser runPx >>= f = Parser $ \l -> case runPx l of
    Nothing      -> Nothing
    Just (x, l') -> runParser (f x) l'

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  Parser runP1 <|> Parser runP2 = Parser $ \l -> case runP1 l of
    res@(Just (_, _)) -> res
    Nothing           -> runP2 l
