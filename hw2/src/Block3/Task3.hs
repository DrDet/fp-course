module Block3.Task3
  (
    parserCBS
  , parserDigit
  ) where

import           Block3.Task1        (Parser (..))
import           Block3.Task2        (element, eof, ok, satisfy)
import           Control.Applicative (some, (<|>))
import           Data.Char           (digitToInt, isDigit)

term :: Char -> Parser Char ()
term c = const () <$> element c

parserCBS :: Parser Char ()
parserCBS = s >> eof
  where
    s       = (leftBr >> s >> rightBr >> s) <|> ok
    leftBr  = term '('
    rightBr = term ')'

digit :: Parser Char Int
digit = digitToInt <$> (satisfy isDigit)

number :: Parser Char Int
number = foldl (\acc d -> 10 * acc + d) 0 <$> some digit

sign :: Parser Char (Int -> Int)
sign = (const (* 1) <$> (element '+')) <|> (const (* (-1)) <$> (element '-')) <|> (const id <$> ok)

parserDigit :: Parser Char Int
parserDigit = s >>= (\n -> (const n) <$> eof)
  where
    s = sign >>= \f -> f <$> number
