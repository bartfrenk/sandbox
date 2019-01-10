module Zeta.Interpreter.Internal where

import           Control.Monad.Except   (throwError)
import           Control.Monad.State
import           Data.Map               ((!?))
import qualified Data.Map               as Map
import           Lens.Micro
import           Lens.Micro.Mtl         hiding (assign)
import           Zeta.Interpreter.Types
import           Zeta.Syntax


execute :: Domain m => Expr -> InterpreterT m Expr
execute literal@(Literal _) = pure literal
execute external@(External _) = pure external
execute (Binding bs expr) =
  let unwrapped = runInterpreterT (execute expr)
  in InterpreterT $ withStateT (over bindings (`Map.union` bs)) unwrapped
execute (App fn args) = do
  fn' <- execute fn
  args' <- traverse execute args
  case fn' of
    External urn -> do
      externals' <- view externals
      let signature = (urn, Map.keysSet args')
      case externals' !?  signature of
        Nothing ->
          throwError $ MissingExternal signature
        Just expr ->
          let unwrapped = runInterpreterT (execute expr)
          in InterpreterT $ withStateT (set bindings args') unwrapped
    expr ->
      throwError $ InvalidFunction expr
execute (Fetch urn args) = do
  args' <- traverse execute args
  case traverse maybeLiteral args' of
    Nothing ->
      throwError $ InvalidFetch urn args'
    Just literals -> do
      fetch' <- view fetch
      lift (fetch' urn literals)
  where
    maybeLiteral (Literal lit) = Just lit
    maybeLiteral _ = Nothing
execute (Assignment name expr) = do
  expr' <- execute expr
  assign name expr'
  pure $ Literal None
execute (BinaryOp op expr1 expr2) = do
  expr1' <- execute expr1
  expr2' <- execute expr2
  case (expr1', expr2') of
    (Literal lit1, Literal lit2) ->
      Literal <$> operation op lit1 lit2
    (Literal _, _) ->
      throwError $ UnevaluatedExpression expr2'
    (_, _) ->
      throwError $ UnevaluatedExpression expr1'
execute (Var name) = do
  bindings' <- use bindings
  case bindings' !? name of
    Nothing ->
      throwError $ NonExistentBinding name
    Just expr -> execute expr
execute (Sequence expr1 expr2) =
  execute expr1 >> execute expr2

interpret :: Domain m => Expr -> InterpreterT m Expr
interpret expr = undefined

operation :: Domain m => BinaryOp -> Literal -> Literal -> InterpreterT m Literal
operation = undefined
