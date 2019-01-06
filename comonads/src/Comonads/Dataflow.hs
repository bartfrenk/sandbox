{-# LANGUAGE TypeOperators #-}
module Comonads.Dataflow where

import Comonads.Types (Comonad(..))

data e :& a = e :& a


instance Functor ((:&) e) where
  f `fmap` (e :& a) = e :& f a

instance Comonad ((:&) e) where
  extract (_ :& a) = a

ask :: a :& e -> e
ask (_ :& e) = e

local :: (e -> e) -> e :& a -> e :& a
local g (e :& a) = (g e) :& a

data FunArg s a = (s -> a) :# s

instance Functor (FunArg s) where
  f `fmap` (g :# s) = (f . g) :# s

instance Comonad (FunArg s) where
  extract (g :# s) = g s
  extend k (f :# s) = (\s' -> k (f :# s')) :# s

data List a = Nil | List a :> a

data LV a = List a := a

data Stream a = a :< Stream a

instance Functor Stream where
  f `fmap` (a :< str) = (f a) :< (f `fmap` str)

data LVS a = LV a :| Stream a


-- instance Comonad LVS  where
--   extract (_ := a :| _) = a
--   extend k d = extendLeft d := k d :| extendRight d
--     where
--       extendLeft (Nil := a :| as) = Nil
--       extendLeft (az' :> a' := as) = undefined

-- Introduction
-- What is a monad?
-- What is a comonad?
-- 
