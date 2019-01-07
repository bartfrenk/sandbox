{-# LANGUAGE TypeFamilies #-}
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
  | Var Name
  | Resolver URN
  deriving (Eq, Show)

data Literal
  = I Int
  | B Bool
  deriving (Eq, Show)
