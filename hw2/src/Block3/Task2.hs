module Block3.Task2
  (
    ok
  , eof
  , satisfy
  , element
  , stream
  ) where

import           Block3.Task1 (Parser (..))

ok :: Parser s ()
ok = Parser $ \l -> Just((), l)

eof :: Parser s ()
eof = Parser $ \l -> case l of
  [] -> Just((), [])
  _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \l -> case l of
  []       -> Nothing
  (x : xs) -> if p x
    then Just (x, xs)
    else Nothing

element :: Eq s => s -> Parser s s
element e = satisfy (== e)

stream :: Eq s => [s] -> Parser s [s]
stream = mapM element
