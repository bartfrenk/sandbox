module Zeta.Syntax where

import           Data.Map.Strict (Map)
import           Data.String     (IsString (..))
import           Data.Text       (Text)
import qualified Data.Text       as T
import           GHC.Exts        (IsList (..))

newtype Name = Name Text deriving (Eq, Ord, Show)

instance IsString Name where
  fromString = Name . T.pack

newtype URN = URN [Text] deriving (Eq, Ord, Show)

instance IsList URN where
  type Item URN = Text
  fromList = URN
  toList (URN ts) = ts

data Expr
  = Literal Literal
  | Binding (Map Name Expr) Expr
  | App Expr (Map Name Expr)
  | External URN
  | Fetch URN (Map Name Expr)
  | Assignment Name Expr
  | BinaryOp BinaryOp Expr Expr
  | Var Name
  | Sequence Expr Expr

data Literal
  = I Int
  | B Bool
  | None

data BinaryOp
  = LessThen
  | GreaterThen
  | Equals
  | LessThenOrEqual
  | GreaterThenOrEqual
  | Times
  | Plus
