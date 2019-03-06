module Task2
  (
    doubleNeg
  , thirdNegElim
  , excludedNeg
  , pierce
  , doubleNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
-- a -> (a -> Void) -> Void
doubleNeg x f = f x

-- Не получилось заселить, но тип обитаемый
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = undefined

-- Не доказуемо
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- Не доказуемо
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
-- (((a -> Void) -> Void) -> Void) -> a -> Void
thirdNegElim f x = f (doubleNeg x)
