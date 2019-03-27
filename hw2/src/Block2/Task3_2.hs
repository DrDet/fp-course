{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE UndecidableInstances #-}

module Block2.Task3_2
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

instance Monad m => MonadJoin m where
  returnJoin :: a -> m a
  returnJoin = return

  join       :: m (m a) -> m a
  join mma = mma >>= id
