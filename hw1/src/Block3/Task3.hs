module Block3.Task3
  (
    Nat(..)
  , add
  , mul
  , sub
  , toNat
  , fromNat
  , isEven
  , divide
  , modulo
  ) where

data Nat = Z | S Nat
  deriving Show

add :: Nat -> Nat -> Nat
add Z y     = y
add (S x) y = S (add x y)

mul :: Nat -> Nat -> Nat
mul Z _     = Z
mul (S x) y = add y (mul x y)

sub :: Nat -> Nat -> Nat
sub Z _         = Z
sub x Z         = x
sub (S x) (S y) = sub x y

toNat :: Int -> Nat
toNat n
  | n <= 0    = Z
  | otherwise = S (toNat (n - 1))

fromNat :: Nat -> Int
fromNat Z     = 0
fromNat (S x) = 1 + fromNat x

instance Eq Nat where
  x == y = compare x y == EQ

instance Ord Nat where
  compare Z Z         = EQ
  compare _ Z         = GT
  compare Z _         = LT
  compare (S x) (S y) = compare x y

isEven :: Nat -> Bool
isEven x = x `modulo` two == Z
  where two = toNat 2

divide :: Nat -> Nat -> Nat
divide x y
  | x < y     = Z
  | otherwise = S ((x `sub` y) `divide` y)

modulo :: Nat -> Nat -> Nat
modulo x y
  | x < y     = x
  | otherwise = (x `sub` y) `modulo` y
