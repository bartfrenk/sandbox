{-# LANGUAGE ExistentialQuantification #-}
module ThereIsNoFork where

import Control.Monad.Free
import Control.Concurrent
import Data.IORef


data Fetch a = Done a | Blocked (Fetch a)

-- 'Fetch' is the free monad over FetchF
data FetchF a b = DoneF a | BlockedF b

instance Functor Fetch where
  f `fmap` Done a = Done (f a)
  f `fmap` Blocked cx = Blocked (f `fmap` cx)

instance Applicative Fetch where
  pure = Done
  Done f <*> Done x = Done (f x)
  Done f <*> Blocked cx = Blocked $ f `fmap` cx
  Blocked cf <*> Done x = Blocked $ (\z -> z x) `fmap` cf
  Blocked cf <*> Blocked cx = Blocked (cf <*> cx)

instance Monad Fetch where
  return = Done
  Done x >>= f = f x
  Blocked cx >>= f = Blocked (cx >>= f)



-- Definition of app
ap' :: Monad m => m (a -> b) -> m a -> m b
ap' m1 m2 = do { f <- m1; x <- m2; return $ f x }

{-
ap mf mx
= { definition of ap }
do { f <- mf; x <- mx; return $ f x }
= { desugar do-notation }
mf >>= (\f -> (mx >>= \x -> return $ f x))

ap (Done f) (Done x)
=
Done f >>= (\f -> (Done x >>=
-}


data Request a

data BlockedRequest =
  forall a . BlockedRequest (Request a) (IORef (FetchStatus a))


data FetchStatus a
  = NotFetched
  | FetchSuccess a
