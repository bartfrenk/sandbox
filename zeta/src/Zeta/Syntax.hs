module Zeta.Syntax where

import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)

newtype Name = Name Text deriving (Eq, Ord, Show)

instance IsString Name where
  fromString = Name . T.pack

type Program = [Statement]

data BinaryOp
  = OpEQ
  | OpLE
  | OpGE
  | OpGT
  | OpLT
  deriving (Eq, Show)

data Statement
  = Assignment Name Expr
  | Binding Name Expr
  deriving (Eq, Show)

data Expr
  = BinaryOp BinaryOp Expr Expr
  | Literal Literal
  deriving (Eq, Show)

data Literal
  = I Int
  | B Bool
  deriving (Eq, Show)
