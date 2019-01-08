{-# LANGUAGE TemplateHaskell #-}
module Zeta.Interpreter.Internal where

{-
Note that the existence of user-defined bindings together with the notion of
replacing resolver invocations with AST pieces defined by the interpreter
requires some care, since we do not want bindings to conflict. There is an easy
way out, by evaluating the replaced expressions with an empty map of bindings,
see `withStateT`.
-}

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map.Strict      (Map, (!?))
import qualified Data.Map.Strict      as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Lens.Micro.Mtl
import           Lens.Micro.TH

import           Zeta.Syntax

data RuntimeError
  = RuntimeError String
  deriving (Eq, Show)

type Signature = (URN, Set Name)

type Substitutions = Map Signature Expr

data Runtime = Runtime
  { _substitutions :: Substitutions }

emptyRuntime :: Runtime
emptyRuntime = Runtime { _substitutions = Map.empty }

data Env = Env
  { _bindings :: !(Map Name Expr)
  }

makeLenses ''Runtime
makeLenses ''Env

newtype InterpreterT m a = InterpreterT
   { runInterpreterT :: StateT Env (ExceptT RuntimeError (ReaderT Runtime m)) a
   } deriving (Functor, Applicative, Monad, MonadError RuntimeError,
               MonadState Env, MonadReader Runtime)

interpret :: Monad m => Runtime -> Program -> m (Either RuntimeError Program)
interpret runtime = flip runReaderT runtime . runExceptT .
                    flip evalStateT env . runInterpreterT . interpretProgram
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

getBinding :: Monad m => Name -> InterpreterT m Expr
getBinding name = do
  bs <- gets _bindings
  case bs !? name of
    Nothing -> throwError $ RuntimeError $ "unbound variable " ++ show name
    Just expr -> pure expr

interpretExpr :: Monad m => Expr -> InterpreterT m Expr
interpretExpr (BinaryOp op e1 e2) = do
  e1' <- interpretExpr e1; e2' <- interpretExpr e2
  case (e1', e2') of
    (Literal lit1, Literal lit2) -> interpretBinaryOp op lit1 lit2
    _ -> throwError $ RuntimeError "cannot reduce operands"
interpretExpr x@(Literal _) = pure x
interpretExpr (Var name) = getBinding name
interpretExpr expr@(Resolver _) = pure expr
interpretExpr (App (Resolver urn) argList) = do
  argList' <- interpretExpr `mapM` argList
  let names = argNames argList
  subst <- view substitutions
  case subst !? (urn, names) of
    Nothing -> throwError $ RuntimeError ("resolver " ++ show urn ++ " unavailable")
    Just expr -> interpretInScope (toBindings argList') expr
interpretExpr (App var@(Var _) argList) = do
  var' <- interpretExpr var
  interpretExpr (App var' argList)
interpretExpr _ = throwError $ RuntimeError "cannot apply non-resolvers"

interpretInScope :: Monad m => Map Name Expr -> Expr -> InterpreterT m Expr
interpretInScope local expr = InterpreterT $ withStateT
  (const $ Env local) (runInterpreterT (interpretExpr expr))

interpretBinaryOp :: Monad m => BinaryOp -> Literal -> Literal -> InterpreterT m Expr
interpretBinaryOp op (I n1) (I n2) = pure (Literal $ B (n1 `fn` n2))
  where fn = case op of
          OpEQ -> (==)
          OpLE -> (<=)
          OpGE -> (>=)
          OpGT -> (>)
          OpLT -> (<)
interpretBinaryOp _ _ _ = throwError $ RuntimeError "invalid operation"
