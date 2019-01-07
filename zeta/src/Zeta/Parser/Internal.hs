module Zeta.Parser.Internal where

import           Control.Monad.Reader
import           Data.Functor.Identity
import           Data.Text             as T
import           Text.Parsec
import           Text.Parsec.Indent
import Text.Parsec.Expr


import           Zeta.Parser.Lexer
import           Zeta.Syntax

type Parser s a = IndentParser s () a

type CharStream s = Stream s (IndentT Identity) Char

parse :: Text -> Either ParseError Program
parse = runIndentParser (program <* eof) () ""

program :: CharStream s => Parser s Program
program = whitespace *> block statement

statement :: CharStream s => Parser s Statement
statement = binding <|> assignment

assignment :: CharStream s => Parser s Statement
assignment = Assignment <$> (name <* assign) <*> expr

binding :: CharStream s => Parser s Statement
binding =
  try $ reserved "let" *>
  (Binding <$> (name <* assign) <*> expr)

term :: CharStream s => Parser s Expr
term = parens expr <|> intLiteral <|> variable <?> "term"

intLiteral :: CharStream s => Parser s Expr
intLiteral = Literal . I <$> integer

variable :: CharStream s => Parser s Expr
variable = Var <$> name

expr :: CharStream s => Parser s Expr
expr = buildExpressionParser table term <?> "expression"
  where
    table = [[ Infix (op "==" >> (pure $ BinaryOp OpEQ)) AssocLeft
             , Infix (op "<"  >> (pure $ BinaryOp OpLT)) AssocLeft
             , Infix (op ">"  >> (pure $ BinaryOp OpGT)) AssocLeft
             , Infix (op "<=" >> (pure $ BinaryOp OpLE)) AssocLeft
             , Infix (op ">=" >> (pure $ BinaryOp OpGE)) AssocLeft
             ]]

