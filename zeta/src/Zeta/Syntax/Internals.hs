module Zeta.Syntax.Internals where

import           Data.Hashable
import           Data.Map.Strict (Map)
import           Data.String     (IsString (..))
import           Data.Text       (Text)
import qualified Data.Text       as T
import           GHC.Generics
import           Prelude
import GHC.Exts

import           Zeta.Types

newtype Name = Name Text deriving (Eq, Ord, Show, Hashable)

instance IsString Name where
  fromString = Name . T.pack

data Expr
  = Literal Literal
  | Binding (Map Name Expr) Expr
  | App Expr (Map Name Expr)
  | External URN
  | Fetch URN
  | Assignment Name Expr
  | BinaryOp BinaryOp Expr Expr
  | Var Name
  | Sequence Expr Expr
  | Empty
  deriving (Eq, Show)

instance IsList Expr where
  type Item Expr = Expr

  fromList [] = Empty
  fromList [x] = x
  fromList (x:xs) = Sequence x (fromList xs)
  fromList _ = error "should not happen"

  toList Empty = []
  toList (Sequence e1 e2) = toList e1 ++ toList e2
  toList e = [e]

instance IsString Expr where
  fromString = Literal . fromString

data Literal
  = None
  | I Int
  | B Bool
  | S Text
  deriving (Eq, Ord, Show, Generic)

instance Hashable Literal

instance IsString Literal where
  fromString = S . T.pack

instance TemplateParam Literal where
  asParam None = Nothing
  asParam (I n) = Just (T.pack $ show n)
  asParam (B b) = Just (T.pack $ show b)
  asParam (S t) = Just t

data BinaryOp
  = LessThan
  | GreaterThan
  | LessThanOrEqual
  | GreaterThanOrEqual
  | Equals
  | Times
  | Plus
  deriving (Eq, Show)
