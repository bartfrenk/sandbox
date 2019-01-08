{-# LANGUAGE StrictData #-}
module Interpreter.Monadic where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Text (Text)
import Data.Set (Set)

data RuntimeError
  = MissingExternal ExtSignature
  | MissingBinding Name
  | RuntimeError String

type Updates = Map Name Expr

class Monad m => Domain m where
  assign :: Name -> Expr -> m ()
  raise :: RuntimeError -> m ()


instance Domain m => Domain (InterpreterT m) where
  raise err = lift $ raise err
  assign name expr = lift $ assign name expr

newtype InterpreterT m a = InterpreterT
  { runInterpreterT ::
      StateT RuntimeState
      (ExceptT RuntimeError
        (ReaderT (RuntimeEnv m) m)) a
  } deriving (Functor, Applicative, Monad, MonadState RuntimeState)

instance MonadTrans InterpreterT where
  lift = InterpreterT . lift . lift . lift

newtype RuntimeState = RuntimeState
  { bindings :: Map Name Expr
  }

type ExtSignature = (URN, Set Name)

data RuntimeEnv m = RuntimeEnv
  { externals :: Map ExtSignature Expr
  , fetch :: URN -> Map Name Literal -> m Expr }


reduce :: Monad m => Expr -> InterpreterT m Expr
reduce = undefined

interpret :: Domain m => Expr -> InterpreterT m Expr
interpret expr = reduce expr >>= \case
  _ -> undefined


newtype Name = Name Text

newtype URN = URN [Text]

data Expr
  = Literal Literal
  | Binding (Map Name Expr) Expr
  | App Function (Map Name Expr)
  | Assignment Name Expr
  | BinaryOp BinaryOp Expr Expr

newtype Function
  = External URN

data BinaryOp
  = LessThen
  | GreaterThen
  | Equals
  | LessThenOrEqual
  | GreaterThenOrEqual
  | Times
  | Plus


data Literal = I Int | B Bool | None
