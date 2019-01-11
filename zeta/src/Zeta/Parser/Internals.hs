module Zeta.Parser.Internals where

-- import           Data.Functor.Identity

-- import qualified Data.Map              as Map
import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Parsec
-- import           Text.Parsec.Expr
-- import           Text.Parsec.Indent


-- import           Zeta.Parser.Lexer
import           Zeta.Syntax

-- type Parser s a = IndentParser s () a

-- type CharStream s = Stream s (IndentT Identity) Char

parse :: Text -> Either ParseError Expr
parse = undefined


-- parse :: Text -> Either ParseError Program
-- parse = runIndentParser (program <* eof) () ""

-- program :: CharStream s => Parser s Program
-- program = whitespace *> block statement

-- statement :: CharStream s => Parser s Statement
-- statement = binding <|> assignment

-- assignment :: CharStream s => Parser s Statement
-- assignment = Assignment <$> (name <* assign) <*> expr

-- binding :: CharStream s => Parser s Statement
-- binding =
--   try $ reserved "let" *>
--   (Binding <$> (name <* assign) <*> expr)

-- term :: CharStream s => Parser s Expr
-- term = parens expr <|> intLiteral <|> app <|> resolver <|> variable <?> "term"

-- intLiteral :: CharStream s => Parser s Expr
-- intLiteral = Literal . I <$> integer

-- variable :: CharStream s => Parser s Expr
-- variable = Var <$> name

-- resolver :: CharStream s => Parser s Expr
-- resolver = Resolver <$> try (reserved "resolver" *> parens urn)

-- app :: CharStream s => Parser s Expr
-- app = try (App <$> (resolver <|> variable) <*> parens argList)

-- argList :: CharStream s => Parser s (ArgList Expr)
-- argList = ArgList . Map.fromList <$> (arg `sepBy` symbol ",")
--   where arg = (,) <$> (name <* symbol "=") <*> expr

-- expr :: CharStream s => Parser s Expr
-- expr = buildExpressionParser table term <?> "expression"
--   where
--     table =
--       [[comp "==" OpEQ,
--         comp "<=" OpLE, comp "<" OpLT,
--         comp ">=" OpGE, comp ">" OpGT]]
--     comp s d = Infix (op s >> pure (BinaryOp d)) AssocLeft