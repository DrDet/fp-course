module Block2.Task1
  (
    Expr(..)
  , ArithmeticError(..)
  , eval
  ) where

data Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving Show

data ArithmeticError
  = DivisionByZero
  | NegativePower
  deriving (Show, Eq)

eval :: Expr -> Either ArithmeticError Int
eval expr =
  case expr of
    (Const x) -> return x
    (Add a b) -> calc (+) Nothing a b
    (Sub a b) -> calc (-) Nothing a b
    (Mul a b) -> calc (*) Nothing a b
    (Div a b) -> calc quot (Just ((/= 0), DivisionByZero)) a b
    (Pow a b) -> calc (^) (Just ((>= 0), NegativePower)) a b
  where
    calc :: (Int -> Int -> Int) -> Maybe (Int -> Bool, ArithmeticError) -> Expr -> Expr -> Either ArithmeticError Int
    calc op Nothing a b = do
      x <- eval a
      y <- eval b
      return (x `op` y)
    calc op (Just (check, e)) a b = do
      x <- eval a
      y <- eval b
      if check y
      then return (x `op` y)
      else Left e
