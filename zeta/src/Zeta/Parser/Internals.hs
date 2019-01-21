module Zeta.Parser.Internals where

import           Data.Functor.Identity
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Indent

import           Zeta.Parser.Lexer
import           Zeta.Syntax

type Parser s a = IndentParser s () a

type CharStream s = Stream s (IndentT Identity) Char

parse :: Text -> Either ParseError Expr
parse = runIndentParser (toSequence =<< block expr) () ""

expr :: CharStream s => Parser s Expr
expr = buildExpressionParser table term <?> "expression"
  where
    table =
      [ [ binary "*" Times ]
      , [ binary "+" Plus ]
      , [ binary "==" Equals,
          binary "<=" LessThanOrEqual, binary "<" LessThan,
          binary ">=" GreaterThanOrEqual, binary ">" GreaterThan]]
    binary s d = Infix (op s >> pure (BinaryOp d)) AssocLeft

term :: CharStream s => Parser s Expr
term = parens expr <|>
       try app <|>
       try assignment <|>
       external <|>
       fetch <|>
       binding <|>
       literal <|>
       var

var :: CharStream s => Parser s Expr
var = Var <$> name

literal :: CharStream s => Parser s Expr
literal = noneLiteral <|> intLiteral <|> boolLiteral <|> stringLiteral
  where
    intLiteral = Literal . I <$> integer
    boolLiteral = true <|> false
    true = reserved "True" >> pure (Literal $ B True)
    false = reserved "False" >> pure (Literal $ B False)
    stringLiteral =
      let s = char '"' *> many (noneOf "\"") <* char '"'
      in Literal . S . T.pack <$> s
    noneLiteral = reserved "None" >> pure (Literal None)

-- |Helper function to convert a list of expressions to an expression. Fails
-- when the list is empty.
toSequence :: [Expr] -> Parser s Expr
toSequence [] = fail "expected an expression"
toSequence [e] = pure e
toSequence (e:rest) = Sequence e <$> toSequence rest

binding :: CharStream s => Parser s Expr
binding = Binding <$> letBlock <*> inBlock
  where
    letBlock = Map.fromList <$> withBlock' (reserved "let") bindExpr
    inBlock = toSequence =<< withBlock' (reserved "in") expr
    bindExpr = (,) <$> (name <* op "=") <*> expr

external :: CharStream s => Parser s Expr
external = External <$> (reserved "external" *> parens urn)

fetch :: CharStream s => Parser s Expr
fetch = Fetch <$> (reserved "fetch" *> parens urn)

app :: CharStream s => Parser s Expr
app = App <$> (try external <|> var) <*> parens argList

argList :: CharStream s => Parser s (Map Name Expr)
argList = Map.fromList <$> sepBy1 argPair (symbol ",")
  where argPair = (,) <$> (name <* op "=") <*> expr

assignment :: CharStream s => Parser s Expr
assignment = Assignment <$> (name <* op "=") <*> expr

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


-- variable :: CharStream s => Parser s Expr
-- variable = Var <$> name

-- resolver :: CharStream s => Parser s Expr
-- resolver = Resolver <$> try (reserved "resolver" *> parens urn)

-- app :: CharStream s => Parser s Expr
-- app = try (App <$> (resolver <|> variable) <*> parens argList)

-- argList :: CharStream s => Parser s (ArgList Expr)
-- argList = ArgList . Map.fromList <$> (arg `sepBy` symbol ",")
--   where arg = (,) <$> (name <* symbol "=") <*> expr

