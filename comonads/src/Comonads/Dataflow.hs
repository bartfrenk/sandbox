{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Comonads.Dataflow where

import           Comonads.Types (Comonad (..))

data FunArg s a = (s -> a) :# s

instance Functor (FunArg s) where
  f `fmap` (g :# s) = (f . g) :# s

instance Comonad (FunArg s) where
  extract (g :# s) = g s
  extend k (f :# s) = (\s' -> k (f :# s')) :# s

-- |A stream is a list of values that goes on forever. The symbol for the data
-- constructor is chosen so that it points to the designated index in the
-- 'PointedStream' data type.
data Stream a = a :< Stream a
-- |A snoc list. The symbol for the data constructor is chosen so that that it
-- points to the designated index in the 'PointedStream' data type.
--
-- This data type is called LV in the article.
data List a = Nil | List a :> a
data NonEmptyList a = List a := a
-- |A PointedStream is a stream with a designated index. Conceptually we think
-- of the designated index as now, everything coming before the designated index
-- as history, and everything after the designated index as future.
--
-- This data type is called LVS in the article.
data PointedStream a = NonEmptyList a :| Stream a

infixl 9 :<
infixl 9 :>
infixl 8 :=
infixl 8 :|

instance Functor List where
  f `fmap` Nil = Nil
  f `fmap` (xs :> x) = (f `fmap` xs) :> f x

instance Functor Stream where
  f `fmap` (a :< str) = f a :< (f `fmap` str)

instance Functor NonEmptyList where
  fmap f (xs := x) = (f `fmap` xs) := f x

instance Functor PointedStream where
  fmap f (lv :| str) = (f `fmap` lv) :| (f `fmap` str)

-- |Make the head of the stream the designated index.
point :: Stream a -> PointedStream a
point (x :< xs) = Nil := x :| xs

-- |Forget about the designated index.
unpoint :: PointedStream a -> Stream a
unpoint (Nil := now :| future) = now :< future
unpoint ps = unpoint (regress ps)

-- |@takeStream n stream@ takes the first @n@ elements of @stream@.
takeStream :: Int -> Stream a -> [a]
takeStream n (x :< xs)
  | n <= 0 = []
  | n > 0 = x:takeStream (n - 1) xs

nats :: Stream Integer
nats = iterateStream (+ 1) 0

-- |Like Prelude's iterate, but for out 'List' data type.
iterateList :: (a -> a) -> a -> List a
iterateList f init = iterateList f (f init) :> init

-- |Idem, but for 'Stream'.
iterateStream :: (a -> a) -> a -> Stream a
iterateStream f init = init :< iterateStream f (f init)

-- |Move the designated index one position into the future.
advance :: PointedStream a -> PointedStream a
advance (history := now :| next :< future) = history :> now := next :| future

-- |Move the designated index one position into the past.
regress :: PointedStream a -> PointedStream a
regress (history :> last := now :| future) = history := last :| now :< future

instance Comonad PointedStream where
  extract (_ := now :| _) = now
  extend k ps = history := now :| future
    where
      history :> now = k `fmap` iterateList regress ps
      _ :< future = k `fmap` iterateStream advance ps

instance Comonad NonEmptyList where
  extract (_ := now) = now
  extend k xs@(history := _) = f history := k xs
    where
      f Nil = Nil
      f (zs :> z) = f zs :> k (zs := z)

-- |Translate a 'PointedStream' coKleisli arrow to a function on streams. It it
-- interesting to note that this function is surjective, i.e., any stream
-- function is the image of a coKleisli arrow under this map.
runPointedStream :: (PointedStream a -> b) -> Stream a -> Stream b
runPointedStream k stream = k <$> iterateStream advance (point stream)

-- |The image of this function only has causal stream functions.
runNonEmptyList :: (NonEmptyList a -> b) -> Stream a -> Stream b
runNonEmptyList f = runPointedStream $ \(history :| future) -> f history

-- |Takes a value and returns a Kleisli arrow for 'PointedStream' that maps to
-- the 'fby' function on streams under 'runPointedStream'.
fby' :: a -> (NonEmptyList a -> a)
fby' x (Nil := _) = x
fby' _ (_ :> x' := _) = x'

next' :: PointedStream a -> a
next' (_ := _ :| x :< _) = x

fby :: a -> Stream a -> Stream a
fby = runNonEmptyList . fby'

next :: Stream a -> Stream a
next = runPointedStream next'

-- THe basic idea is that there is a natural bijection between stream functions
-- (i.e., functions Stream a -> Stream b) to functions FunArg Nat a -> b. Here
-- FunArg Nat a is the comonad of functions from Nat to a, paired with a
-- distinguished element from Nat. Composition of CoKleisli arrows of this
-- comonad matches composition of stream functions.

-- FunArg Nat a is isomorphic to (List a x a) x Stream a

-- instance Comonad PointedStream  where
--   extract (_ := a :| _) = a
--   extend k d = extendLeft d := k d :| extendRight d
--     where
--       extendLeft (Nil := a :| as) = Nil
--       extendLeft (az' :> a' := as) = undefined

-- Introduction
-- What is a monad?
-- What is a comonad?
--
