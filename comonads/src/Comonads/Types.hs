{-# LANGUAGE TypeSynonymInstances #-}
module Comonads.Types where

-- Definitions and basic properties
-- ================================


-- TODO: swap the arguments of extend to be in line with the definition in Control.Comonad
-- Comonads are dual to monads.
class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  -- One intuition here is that 'extend' transforms a map from contextual value
  -- to contextless values (out of context), to a map from contextual values to
  -- contextual values. This intuition does not seem to apply to the instance
  -- for (->) r.

  -- Note that the definition of extend in 'Control.Comonad' switches the
  -- arguments. This is more in line with the intuition above, i.e., 'extend f'
  -- is the transformed map. The laws also become easier to state.
  extend :: (w a -> b) -> w a -> w b
  extend f = (fmap f) . duplicate

class Category cat where
  identity :: cat a a
  composition :: cat b c -> cat a b -> cat a c

data CoKleisli w a b = CoKleisli (w a -> b)

instance Comonad w => Category (CoKleisli w) where
  identity = CoKleisli extract
  composition (CoKleisli g) (CoKleisli f) = CoKleisli $
    g . extend f

{--
co-Kleisli composition for comonads looks like:

    f <=< g = \x -> f (extend x g)

while for monads it looks like:

    f >=> g = \x -> bind (f x) g

The different forms seem to somewhat surprising at first sight, but probably
easy to understand intuitively if looked at them from the right perspective.

The laws derived from the category laws for CoKleisli are:
--}

prop_leftIdentity ::  (Eq b, Comonad w) => (w a -> b) -> w a -> Bool
prop_leftIdentity f x = extract (extend f x) == f x

prop_rightIdentity :: (Eq b, Comonad w) => (w a -> b) -> w a -> Bool
prop_rightIdentity f x = f (extend extract x) == f x

prop_rightIdentity' :: (Eq (w a), Comonad w) => w a -> Bool
prop_rightIdentity' x = extend extract x == x

-- This does not really seem right.
prop_associativity :: (Eq d, Comonad w) => (w c -> d) -> (w b -> c) -> (w a -> b) -> w a -> Bool
prop_associativity h g f x =
  h (extend (\z -> g (extend f z)) x) == h (extend g (extend f x))

-- Intermezzo: Loeb's Theorem
-- ==========================

{--

From this talk: https://www.youtube.com/watch?v=F7F-BzOB670, referring to Dan
Piponi's blog post: From Löb's Theorem to Spreadsheet Evaluation.

--}

loeb :: Functor f => f (f a -> a) -> f a
loeb fs = xs where xs = fmap ($ xs) fs

{--
loeb [length, (!! 0), \x -> x !! 0 + x !! 1]
= { definition loeb }
fmap ($ xs) [length, (!! 0), \x -> x !! 0 + x !! 1]
= { expand fmap }
[length xs, xs !! 0, xs !! 0 + xs !! 1]
= { expand xs }
[length (fmap ($ xs) [...]), (fmap ($ xs) [...] !! 0), xs !! 0 + xs !! 1]
= { length invariant under fmap }
[3, 3, 6]

loeb [length, sum]
= { definition loeb }
fmap ($ xs) [length, sum]
= { expand fmap }
[length xs, sum xs]
= { length invariant under fmap }
[3, sum xs]
= { expand xs }
[3, sum (fmap ($ xs) [length, sum])]
= { diverges }
...
--}

-- Building comonadic intuition
-- ============================

-- Product
-- -------

-- TODO: read up on Bartosz's remarks on the relation between the Reader monad
-- instance, and this comonad instance. Something with adjoints probably.
instance Comonad ((,) c) where
  -- Get the value, independent from the implicit parameter.
  extract (c, a) = a
  -- Make the implicit parameter explicit.
  duplicate (c, a) = (c, (c, a))
  -- Change the value in the comonad depending on the implicit parameter.
  extend f (c, a) = (c, f (c, a))

-- Automatons
-- ----------

-- From:
-- This blogpost: http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html

-- A universe is an two-sided infinite list, with a central point.  It is a kind
-- of zipper (TODO: follow-up on zippers).  It might be better to call this a
-- pointed line, since it is isomorphic to a function from the pointed integers
-- to 'x'.
data U x = U [x] x [x]

instance Functor U where
  f `fmap` U ws x ys = U (f `fmap` ws) (f x) (f `fmap` ys)

right :: U x -> U x
right (U ws x (y:ys)) = U (x:ws) y ys

left :: U x -> U x
left (U (w:ws) x ys) = U ws w (x:ys)

instance Comonad U where
  extract (U _ x _) = x
  duplicate u = U (tail $ iterate left u) u (tail $ iterate right u)

-- Rules are projections of pointed universe. They determine a value based on
-- the universe and a special point in this universe.
type Rule x = U x -> x


-- Is it so that any indexed structure has a comonad instance.

-- class Indexed i x | x -> i where



-- From:
-- http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html

-- Object oriented programming
-- ---------------------------

{--

First example: a contextual value, in which the context is extensible.

Think about the Builder pattern.

--}


instance Monoid r => Comonad ((->) r) where
  -- Construct a value with minimal context
  extract f = f mempty
  -- Allow extra context.
  duplicate f r1 r2 = f (r1 `mappend` r2)
  -- ?? Not too sure what would be an intuitive description here.
  extend u f r = u (\r' -> f (r `mappend` r))


-- The iterator pattern
-- --------------------

-- Compare this to The Essence of Dataflow Programming

data Iterator a = a :< (Iterator a)
infixr 5 :<

instance Functor Iterator where
  f `fmap` (a :< rest) = (f a) :< f `fmap` rest

initialHistory :: Iterator String
initialHistory = "" :< initialHistory

exampleHistory :: Iterator String
exampleHistory = "^D" :< "^C" :< "hello" :< initialHistory

instance Comonad Iterator where
  extract (a :< _) = a
  duplicate (a :< rest) = (a :< rest) :< duplicate rest
  -- Iterator a -> (Iterator a -> b) -> Iterator b
  extend retrieve iterator = retrieve `fmap` duplicate iterator

next :: Iterator a -> Iterator a
next (_ :< rest) = rest

-- The command pattern (the Store comonad)
-- ---------------------------------------
-- This is the 'Store' comonad (from Control.Comonad.Store), up to ordering of
-- the arguments of 'extend'. There it is formulated as a comonad transformer
-- over the Identity comonad.

newtype Store r a = Store (r, r -> a)

instance Functor (Store r) where
  f `fmap` (Store (r, h)) = Store (r, f . h)

instance Comonad (Store r) where
  extract (Store (r, h)) = h r
  duplicate (Store (r, h)) = Store (r, \r' -> Store (r', h))
  extend f (Store (r, h)) = Store (r, \r' -> f $ Store (r', h))



-- In the comonads package there are three comonad type classes and associated
-- transformers: Env, Store and Traced. The examples above have instances for:
-- 1. our Store is StoreT Identity
-- 2. (,) r is isomorphic to EnvT Identity
-- 3. Monoid r => (->) r is TracedT Identity
-- This leaves Iterator.

-- Dataflow programming
-- --------------------


--

{-- Follow ups ==========

1. https://github.com/kwf/ComonadSheet
(http://hackage.haskell.org/package/ComonadSheet). The author has a talk on
YouTube that might be interesting.

2. Coeffects: Unified static analysis of context-dependence. Fairly technical
paper that talks about tracing co-effects. Seems to be interesting, is not that
long (12 pages), but given the fairly technical nature might take a day or two
to get through.

--}
