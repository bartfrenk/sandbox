{- Initialize the required Haskell environment. -}

module MutableData where

import           Control.Monad.Primitive     (PrimMonad, PrimState)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M
import           System.Random               (StdGen, getStdGen, randomR)
import qualified Data.Vector.Generic.Mutable as G

{-
Implementation of the Fisher-Yates shuffle algorithm. It uniformly samples a
permutation of the input list in time linear in the length of the input
list. Takes the indices of the list, starting from the last, and swaps the
element at that index with an element at a uniform random lower index.

Types of the functions that change the mutable vector are of the form:

(PrimMonad m, Unbox a) => MVector (PrimState m) a -> ... -> m ()

indicating that they run in a monad with a PrimMonad instance, and provide their
functionality through side-effects (changing the supplied MVector).

What is PrimMonad
=================

The PrimMonad type class really only has two instances: IO and ST s. All the
other instances definitions are to ensure that having a PrimMonad instance is
invariant under certain monad transformations (e.g., PrimMonad m => PrimMonad
(StateT s m)).

Providing an PrimMonad instance for a monad m requires one to do two things:

1. Supply the type of an associated 'state token', called PrimState m.

2. Supply a function, called 'primitive' of type:

   (State# (PrimState m) -> (# State# (PrimState m), a#)) -> m a

Question: What is the reason for the existence of the 'PrimBase' class in
Control.Monad.Primitive?

We have PrimState IO = RealWorld, where RealWorld is a deeply magical data
type. Its only purpose is to function as a state token for the PrimMonad
instance of IO. Seen in the light of the PrimMonad instance, sequencing IO
actions translates to function composition in which the state tokens are passed
from one function to the next. This ensures the correct order of execution
of the IO actions.

We have PrimState (ST s) = s. This instance is more general than that for IO,
and it allows for extending certain types of IO actions to the more restricted
(ST s) monad (i.e., exactly those actions that run in a polymorphic monad,
constrained to the PrimMonad type class).

The essence of why (ST s) is interesting lies in its capability of running
actions that update data in place, and the existence of a function with the
type:

  runST :: (forall s. ST s a) -> a.

It takes an action, that for any state token, produces a value of type a, and
returns an (unwrapped) value of type a.

Question: I read that mutations cannot escape the ST monad, and that this is
reflected in the rank-2 type of the runST function. What is the mechanism
exactly? Is there some formal way to express this?

Vectors
=======

The vector package has a root level namespace of Data.Vector. Within that
namespace there are five sub-namespaces:

1. Generic
2. Mutable
3. Primitive
4. Storable
5. Unboxed

Data.Vector.Primitive is not recommended except for very special cases, since
the vector types defined in Data.Vector.Unboxed are significantly more flexible
at no performance cost.

Data.Vector.Storable is required for inter-operating with other languages via
the FFI.

Data.Vector deals with immutable vectors (although it exports the MVector type
for mutable vectors).

Data.Vector.Mutable has functionality for mutable vectors. A mutable vector has
type 'data MVector s a', where s a state token (as discussed earlier in the part
on PrimMonad), and a is the type of the elements of the vector.

Unboxed uses type families to select an efficient representation for
Unboxed.Vector a, that depends on the type a (thus, Unboxed.Vector is a type
family). It also has a type family 'MVector s a' for mutable, unboxed vectors.

Question: How to abstract over all these vector types? Look at
Data.Vector.Generic.

-}

shuffleM' :: (PrimMonad m, G.MVector v a)
          => StdGen
          -> Int -- ^ count to shuffle
          -> v (PrimState m) a
          -> m ()
shuffleM' _ i _ | i <= 1 = return ()
shuffleM' gen i v = do
    G.swap v i' index
    shuffleM' gen' i' v
  where
    (index, gen') = randomR (0, i') gen
    i' = i - 1

shuffleM :: (PrimMonad m, V.Unbox a)
         => StdGen
         -> Int -- ^ count to shuffle
        -> M.MVector (PrimState m) a
         -> m ()
shuffleM _ i _ | i <= 1 = return ()
shuffleM gen i v = do
    M.swap v i' index
    shuffleM gen' i' v
  where
    (index, gen') = randomR (0, i') gen
    i' = i - 1

shuffle :: V.Unbox a
        => StdGen
        -> V.Vector a
        -> V.Vector a
shuffle gen vector = V.modify (shuffleM' gen (V.length vector)) vector

main :: IO ()
main = do
    gen <- getStdGen
    print $ shuffle gen $ V.enumFromTo 1 (20 :: Int)
