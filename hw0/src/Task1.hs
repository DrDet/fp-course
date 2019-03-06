{-# LANGUAGE TypeOperators #-}

module Task1
  (
    distributivity
  , associator
  , eitherAssoc
  ) where

distributivity
    :: Either a (b, c)
    -> (Either a b, Either a c)
distributivity (Left x)           = (Left x, Left x)
distributivity (Right (y, z))     = (Right y, Right z)

associator
  :: (a, (b, c))
  -> ((a, b), c)
associator (x, (y, z)) = ((x, y), z)

type (<->) a b = (a -> b, b -> a)

eitherAssoc
    :: Either a (Either b c)
    <-> Either (Either a b) c
eitherAssoc = (f, f')
  where
    f :: Either a (Either b c) -> Either (Either a b) c
    f (Left x)          = Left (Left x)
    f (Right (Left y))  = Left (Right y)
    f (Right (Right z)) = Right z

    f' :: Either (Either a b) c -> Either a (Either b c)
    f' (Left (Left x))  = Left x
    f' (Left (Right y)) = Right (Left y)
    f' (Right z)        = Right (Right z)
