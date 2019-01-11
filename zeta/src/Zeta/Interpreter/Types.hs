{-# LANGUAGE TemplateHaskell #-}
module Zeta.Interpreter.Types where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Set             (Set)
import           Lens.Micro.TH

import           Zeta.Syntax

data RuntimeError
  = MissingExternal ExtSignature
  | NonExistentBinding Name
  | InvalidOperation BinaryOp Literal Literal
  | UnevaluatedExpression Expr
  | InvalidFunction Expr
  | InvalidFetch URN (Map Name Expr)
  | RuntimeError String
  deriving (Eq, Show)

type Updates = Map Name Expr

class Monad m => Domain m where
  assign :: Name -> Expr -> m ()

instance Domain m => Domain (InterpreterT m) where
  assign name expr = lift $ assign name expr

newtype InterpreterT m a = InterpreterT
  { runInterpreterT ::
      StateT RuntimeState
      (ExceptT RuntimeError
        (ReaderT (RuntimeEnv m) m)) a
  } deriving (Functor, Applicative, Monad,
              MonadState RuntimeState,
              MonadReader (RuntimeEnv m),
              MonadError RuntimeError)

instance MonadTrans InterpreterT where
  lift = InterpreterT . lift . lift . lift

type ExtSignature = (URN, Set Name)

newtype RuntimeState = RuntimeState
  { _bindings :: Map Name Expr
  }

instance Monoid RuntimeState where
  mempty = RuntimeState
    { _bindings = Map.empty
    }
  state1 `mappend` state2 = RuntimeState
    { _bindings = _bindings state1 `Map.union` _bindings state2
    }


-- TODO: Result of fetch should distinguish between different failure scenarios:
-- 1. The URN and argument list do not match a resolver
-- 2. The resolver did not respond
-- Note that the first scenario should be flagged by the type checker
data RuntimeEnv m = RuntimeEnv
  { _externals :: Map ExtSignature Expr
  , _fetch     :: URN -> Map Name Literal -> m (Maybe Expr) }

-- |Left-biased monoid instance for runtime environments.
instance Monad m => Monoid (RuntimeEnv m) where
  mempty = RuntimeEnv
    { _externals = Map.empty
    , _fetch = \_ _ -> pure Nothing
    }
  env1 `mappend` env2 = RuntimeEnv
    { _externals = _externals env1 `Map.union` _externals env2
    , _fetch = \urn literals ->
        _fetch env1 urn literals >>= \case
          Nothing -> _fetch env2 urn literals
          result -> pure result
    }


makeLenses ''RuntimeState
makeLenses ''RuntimeEnv
