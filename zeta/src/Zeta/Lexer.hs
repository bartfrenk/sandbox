module Zeta.Lexer where

import Data.String (fromString)
import           Control.Monad
import           Data.Text         (Text)
import           Text.Parsec       hiding (spaces)
import qualified Text.Parsec       as Parsec
import qualified Text.Parsec.Token as T

import           Zeta.Syntax

type MonadParse s m = (Stream s m Char, Monad m)

type ParserT s m a = ParsecT s () m a

spaces :: MonadParse s m => ParserT s m ()
spaces = void $ many $ oneOf " \t"

whitespace :: MonadParse s m => ParserT s m ()
whitespace = void $ Parsec.spaces

lexeme :: MonadParse s m => ParserT s m a -> ParserT s m a
lexeme p = p <* whitespace

name :: MonadParse s m => ParserT s m Name
name = lexeme $ try $ fromString <$> do
  name <- word
  if isReservedName name
    then unexpected $ "reserved word " ++ show name
    else pure name

assign :: MonadParse s m => ParserT s m String
assign = lexeme $ string "="

op :: MonadParse s m => String -> ParserT s m ()
op = void . symbol

parens :: MonadParse s m => ParserT s m a -> ParserT s m a
parens = between (symbol "(") (symbol ")")

symbol :: MonadParse s m => String -> ParserT s m String
symbol = lexeme . string

word :: MonadParse s m => ParserT s m String
word = (:) <$> letter <*> many alphaNum

isReservedName :: String -> Bool
isReservedName = (`elem` ["let"])

integer :: MonadParse s m => ParserT s m Int
integer = lexeme $ read <$> many1 digit

reserved :: MonadParse s m => String -> ParserT s m String
reserved = symbol
