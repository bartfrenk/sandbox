{-# LANGUAGE TemplateHaskell #-}
module Zeta.Interpreter.Types where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map.Strict      (Map)
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

data RuntimeEnv m = RuntimeEnv
  { _externals :: Map ExtSignature Expr
  , _fetch     :: URN -> Map Name Literal -> m Expr }

makeLenses ''RuntimeState
makeLenses ''RuntimeEnv



-- withRuntimeState :: (RuntimeSt -> RuntimeSt) -> InterpreterT m a -> InterpreterT m a
-- withRuntimeState f act =
--   InterpreterT $ withStateT f (runInterpreterT act)

-- mergeBindings :: Map Name Expr -> InterpreterT m a -> InterpreterT m a
-- mergeBindings bindings = withRuntimeSt $
--   \state -> state { bindings = union
