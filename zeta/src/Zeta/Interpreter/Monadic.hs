{-# LANGUAGE StrictData #-}
module Interpreter.Monadic where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Set (Set)

import Zeta.Syntax

data RuntimeError
  = MissingExternal ExtSignature
  | MissingBinding Name
  | RuntimeError String

type Updates = Map Name Expr

class Domain m where
  update :: Name -> Expr -> m ()
  fail :: RuntimeError -> m ()

type Interpreter m = (MonadState RuntimeState m, MonadReader (RuntimeEnv m) m)

newtype InterpreterT m a = InterpreterT {
  runInterpreterT :: StateT RuntimeState (ReaderT (RuntimeEnv m) m) a
}

newtype RuntimeState = RuntimeState
  { bindings :: Map Name Expr
  }

type ExtSignature = (URN, Set Name)

data RuntimeEnv m = RuntimeEnv
  { externals :: Map ExtSignature Expr
  , fetch :: URN -> Map Name Literal -> m Expr }


interpret :: Domain m => Expr -> InterpreterT m Expr
interpret = undefined

