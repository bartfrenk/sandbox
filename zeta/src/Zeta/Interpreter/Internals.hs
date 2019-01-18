module Zeta.Interpreter.Internals where

import           Control.Monad.Except   (runExceptT, throwError)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Function          ((&))
import           Data.Map               ((!?))
import qualified Data.Map               as Map
import           Lens.Micro
import           Lens.Micro.Mtl         hiding (assign)

import           Zeta.Interpreter.Types
import           Zeta.Syntax



-- |Interprets expression in runtime with modified state, without changing the
-- initial state.
withScope :: Domain m => (RuntimeState -> RuntimeState) -> Expr -> InterpreterT m Expr
withScope f expr = do
  state' <- get
  let unwrapped = runInterpreterT (interpret expr)
  expr <- InterpreterT $ withStateT f unwrapped
  put state'
  pure expr

interpret :: Domain m => Expr -> InterpreterT m Expr
interpret literal@(Literal _) = pure literal
interpret external@(External _) = pure external
interpret (Binding bs expr) = withScope (`mappend` RuntimeState bs) expr
interpret (App fn args) = do
  fn' <- interpret fn
  args' <- traverse interpret args
  case fn' of
    External urn -> do
      externals' <- view externals
      let signature = (urn, Map.keysSet args')
      case externals' !?  signature of
        Nothing ->
          throwError $ MissingExternal signature
        Just expr ->
          withScope (set bindings args') expr
    expr ->
      throwError $ InvalidFunction expr
interpret (Fetch urn args) = do
  args' <- traverse interpret args
  case traverse maybeLiteral args' of
    Nothing ->
      throwError $ InvalidFetch urn args'
    Just literals -> do
      fetch' <- view fetch
      lift (fetch' urn literals) >>= \case
        Nothing -> pure $ Literal None
        Just expr -> interpret expr
  where
    maybeLiteral (Literal lit) = Just lit
    maybeLiteral _ = Nothing
interpret (Assignment name expr) = do
  expr' <- interpret expr
  assign name expr'
  pure $ Literal None
interpret (BinaryOp op expr1 expr2) = do
  expr1' <- interpret expr1
  expr2' <- interpret expr2
  case (expr1', expr2') of
    (Literal lit1, Literal lit2) ->
      Literal <$> operate op lit1 lit2
    (Literal _, _) ->
      throwError $ UnevaluatedExpression expr2'
    (_, _) ->
      throwError $ UnevaluatedExpression expr1'
interpret (Var name) = do
  bindings' <- use bindings
  case bindings' !? name of
    Nothing ->
      throwError $ NonExistentBinding name
    Just expr -> interpret expr
interpret (Sequence expr1 expr2) =
  interpret expr1 >> interpret expr2

operate :: Domain m => BinaryOp -> Literal -> Literal -> InterpreterT m Literal
operate LessThan (I m) (I n) = pure $ B (m < n)
operate GreaterThan (I m) (I n) = pure $ B (m > n)
operate LessThanOrEqual (I m) (I n) = pure $ B (m <= n)
operate GreaterThanOrEqual (I m) (I n) = pure $ B (m >= n)
operate Equals (I m) (I n) = pure $ B (m == n)
operate Equals (B b) (B c) = pure $ B (b == c)
operate Times (I m) (I n) = pure $ I (m * n)
operate Plus (I m) (I n) = pure $ I (m + n)
operate op lit1 lit2 = throwError $ InvalidOperation op lit1 lit2


-- |Convenience function to run an interpreter with default initial state.
execute :: Domain m => RuntimeEnv m -> Expr -> m (Either RuntimeError Expr)
execute env expr =
  interpret expr &
  runInterpreterT &
  flip evalStateT mempty &
  runExceptT &
  flip runReaderT env
