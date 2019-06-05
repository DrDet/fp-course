{-# LANGUAGE RankNTypes #-}

module Task5
  (
    set
  , view
  , over
  , (.~)
  , (^.)
  , (%~)
  , lens
  , _1
  , _2
  , choosing
  , (<%~)
  , (<<%~)
  ) where

import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Functor.Const (Const(..), getConst)

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
-- forall f . Functor f => (a -> f b) -> s -> f t
-- type Lens' s a  = Lens s s a a
-- forall f . Functor f => (a -> f a) -> s -> f s

set :: Lens s t a b -> b -> s -> t
set l val = over l (const val)

view :: Lens s t a b -> s -> a
view l obj = getConst $ l Const obj

over :: Lens s t a b -> (a -> b) -> s -> t
over l f obj = runIdentity $ l (Identity . f) obj 

(.~) :: Lens s t a b -> b -> s -> t
(.~) = set

(^.) :: s -> Lens s t a b -> a
(^.) obj l = view l obj 

(%~) :: Lens s t a b -> (a -> b) -> s -> t
(%~) = over

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 mf (fs, x) = flip (,) x <$> mf fs

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 mf (x, sn) = (,) x <$> mf sn

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter = \mf obj -> setter obj <$> mf (getter obj)

-- Объединить две линзы в одну, которая работает с Either.
-- Functor f => (a -> f b) -> (Either s1 s2) -> f (Either t1 t2)
choosing :: Lens s1 t1 a b 
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = 
  \mf obj -> case obj of
    Left  o1 -> Left  . (`setter1` o1) <$> (mf $ getter1 o1) 
    Right o2 -> Right . (`setter2` o2) <$> (mf $ getter2 o2) 
    where
      (setter1, getter1) = (set l1, view l1)
      (setter2, getter2) = (set l2, view l2)

-- Изменить цель линзы и вернуть новый результат. Постарайтесь
-- реализовать без анонимных функций и определений своих функций
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = (f $ s ^. l, l %~ f $ s)

-- Изменить цель линзы, но вернуть старый результат.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = (s ^. l, l %~ f $ s)