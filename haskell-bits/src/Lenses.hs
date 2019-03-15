{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Lenses where

import Data.Functor.Identity
import Data.Functor.Const
import qualified Control.Lens as Lens
import Control.Lens ((^.))

loop :: Bool
loop = loop

-- Main type. Type s is the type of full the structure, type a is the type of
-- the substructure that is focused on. Type t is the type of the resulting full
-- structure, and type b is the type of the substructure.
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- Updates the substructure of s of type a. Just run the lens over an Identity
-- functor.
over :: Lens s t a b -> (a -> b) -> s -> t
over lens f = runIdentity . lens (Identity . f)

view :: Lens s t a b -> s -> a
view lens = getConst . lens Const

-- Control.Lens.view has a different signature
--
-- view :: MonadReader s m => Getting a s a -> m a
--
-- Replacing the MonadReader by a simple instance,
--
-- view :: Getting a s a -> s -> a
--
-- type Getting r s a = (a -> Const r a) -> s -> Const r s

-- (a -> f c) -> (a, b) -> f (c, b)
_1 :: Lens (a, b) (c, b) a c
_1 f (a, b) = (,b) <$> f a

_head :: Lens [a] [a] a a
_head f (x:xs) = fmap (:xs) (f x)
_head _ [] = undefined

newtype Post = Post String

data User = User
  { name :: String
  , posts :: [Post]
  }

_posts :: Lens User User [Post] [Post]
_posts f User{name, posts} = User name <$> f posts

_name :: Lens User User String String
_name f User{name, posts} = flip User posts <$> f name


-- traverse :: (Applicative f, Traversable t)
--          => a -> f b -> t a -> f (t b)
--
-- Let's simplify
--
-- traverse :: Applicative f => a -> f b -> [a] -> f [b]
--
-- Or in other words,
--
-- traverse :: Lens [a] [b] a b,
--
-- except the f is constrained to an Applicative, not a Functor.


g :: (forall r . (a -> r) -> (s -> r)) -> (s -> a)
g f = f (id :: a -> a)


-- https://github.com/ekmett/lens/wiki/Derivation
-- (.) :: (b -> c) -> ((a -> b) -> (a -> c))


-- (.).(.)
-- (.) :: ((a -> b) -> (a -> c)) -> (d -> (a -> b)) -> (d -> (a -> c))
--
-- (.).(.) :: (b -> c) -> (a -> (a -> b))

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
--
-- (.) :: (b' -> c') -> ((a' -> b') -> (a' -> c'))
-- b = b' -> c'
-- c = ((a' -> b') -> (a' -> c'))

-- (.) (.) :: (a -> b -> c) -> a -> (d -> b) -> d -> c

-- (.)
-- a = b' -> c'
-- b = a' -> b'
-- c = a' -> c'

-- (.)(.)(.) :: (b -> c) -> (d -> a -> b) -> d -> a -> c
