{-# LANGUAGE TemplateHaskell #-}
module Zeta.Interpreter.Internal where

{-
Note that the existence of user-defined bindings together with the notion of
replacing resolver invocations with AST pieces defined by the interpreter
requires some care, since we do not want bindings to conflict. There is an easy
way out, by evaluating the replaced expressions with an empty map of bindings,
see `withStateT`.
-}



import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Functor.Identity
import Control.Monad.Except
import Control.Monad.State
import Lens.Micro.Mtl
import Lens.Micro.TH


import Zeta.Syntax

data RuntimeError
  = RuntimeError String
  deriving (Eq, Show)

data Env = Env
  { _bindings :: !(Map Name Expr)
  }

makeLenses ''Env

newtype InterpreterT m a = InterpreterT
   { runInterpreterT :: ExceptT RuntimeError (StateT Env m) a
   } deriving (Functor, Applicative, Monad, MonadError RuntimeError,
               MonadState Env)

interpret :: Monad m => Program -> m (Either RuntimeError Program)
interpret = flip evalStateT env . runExceptT . runInterpreterT .
            interpretProgram
  where
    env = Env { _bindings = Map.empty }

interpretProgram :: Monad m => Program -> InterpreterT m Program
interpretProgram = (concat <$>) . mapM interpretStatement

interpretStatement :: Monad m => Statement -> InterpreterT m Program
interpretStatement (Assignment name expr) = do
  expr' <- interpretExpr expr
  pure [Assignment name expr']
-- There is no notion of scope for bindings. A binding exists in all statements
-- that follow it.
interpretStatement (Binding name expr) = do
  expr' <- interpretExpr expr
  registerBinding name expr'
  pure []

registerBinding :: Monad m => Name -> Expr -> InterpreterT m ()
registerBinding name expr = do
  bs <- gets _bindings
  case bs !? name of
    Nothing -> bindings %= Map.insert name expr
    Just _ -> throwError $ RuntimeError $ "duplicate binding for " ++ show name

interpretExpr :: Monad m => Expr -> InterpreterT m Expr
interpretExpr (BinaryOp op e1 e2) = do
  e1' <- interpretExpr e1; e2' <- interpretExpr e2
  case (e1', e2') of
    (Literal lit1, Literal lit2) -> interpretBinaryOp op lit1 lit2
interpretExpr x@(Literal _) = pure x

interpretBinaryOp :: Monad m => BinaryOp -> Literal -> Literal -> InterpreterT m Expr
interpretBinaryOp op (I n1) (I n2) = pure (Literal $ B (n1 `fn` n2))
  where fn = case op of
          OpEQ -> (==)
          OpLE -> (<=)
          OpGE -> (>=)
          OpGT -> (>)
          OpLT -> (<)
interpretBinaryOp _ _ _ = throwError $ RuntimeError "invalid operation"
