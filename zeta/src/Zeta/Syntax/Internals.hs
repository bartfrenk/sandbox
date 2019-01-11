module Zeta.Syntax.Internals where

import           Prelude
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
  deriving (Eq, Show)

instance IsString Expr where
  fromString = Literal . fromString

data Literal
  = I Int
  | B Bool
  | S Text
  | None
  deriving (Eq, Show)

instance IsString Literal where
  fromString = S . T.pack

data BinaryOp
  = LessThan
  | GreaterThan
  | LessThanOrEqual
  | GreaterThanOrEqual
  | Equals
  | Times
  | Plus
  deriving (Eq, Show)
