{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE UndecidableInstances #-}

module Block2.Task3_1
  (
    MonadFish
  , MonadJoin
  , Monad
  , returnFish
  , returnJoin
  , return
  , (>=>)
  , join
  , (>>=)
  ) where

import           Prelude hiding (Monad, return, (>>=))

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    return :: a -> m a

instance MonadFish m => MonadJoin m where
  returnJoin :: a -> m a
  returnJoin = returnFish

  join       :: m (m a) -> m a
  join = id >=> id

instance MonadFish m => Monad m where
  return :: a -> m a
  return = returnFish

  (>>=)  :: m a -> (a -> m b) -> m b
  ma >>= f = (id >=> f) ma

instance Monad m => MonadFish m where
  returnFish :: a -> m a
  returnFish = return

  (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)
  f >=> g = \a -> f a >>= g
