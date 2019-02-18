module Zeta.Internals where

{-


Th

Overview of the high-level API

parseRuntime :: Text -> IO (Either ParseError Runtime)
parseRuntime = undefined


loadRuntime :: FilePath -> IO (Either ParseError Runtime)
loadRuntime = undefined


execute :: Domain m => Runtime -> Text -> m Expr

executeFromFile :: (MonadIO m, Domain m) => Runtime -> FilePath -> m Expr


What is essentially missing is how to textually represent a RuntimeEnv:
- fetches come from datasources (this already exists)
- externals come from:
  - TODO: datasources (by translating directly to fetches)
  - TODO: definitions in the source text

This depends on Haxl, since the fetches in the RuntimeEnv will run in a lifted
Haxl monad.

There is an issue here with mixing levels: we have InterpreterT and Domain, one
a specific monad stack, the other the typeclass for a constrained monad. Maybe
it is better to have one or the other.

-}

-- Scratch
import Data.Text (Text)

data FetchResult a =
  Success a | Failure Text

data URN

data Arguments

data Signature

data Expr

-- Maybe leave the externals out, since we might want to use the externals
-- mechanism to define functions within the program.
class Monad m => MonadFetch m where
  fetch :: URN -> Arguments -> m (FetchResult a)
