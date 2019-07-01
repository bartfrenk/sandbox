module Data.Config.Internals where

import           Control.Monad.Catch  (Exception, MonadThrow, throwM)
import           Control.Monad.Trans
import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe
import           Data.YAML
import           Data.YAML.Event      (Tag, mkTag)

newtype ParseError = ParseError String deriving (Eq, Show)

instance Exception ParseError


readConfig :: (MonadIO m, MonadThrow m, FromYAML v) => FilePath -> m v
readConfig = undefined

decodeConfig :: (MonadThrow m, FromYAML v) => ByteString -> m v
decodeConfig bs = case decodeNode bs of
  Left description -> throwM $ ParseError description
  Right [Doc v] -> do
    node <- resolve baseResolver v
    case parseEither $ parseYAML node of
      Left description -> throwM $ ParseError description
      Right v          -> pure v
  Right _ -> throwM $ ParseError "Expected a single YAML value"

test :: IO Node
test = decodeConfig example
  where
    example = "host: !or [!env HOST, localhost]\n\
              \port: 123\n\
              \password: !secret password"

-- Not going to work, since we might need to query the environment to know whether
-- a value exists
type Resolver m = (Node -> m Node) -> Node -> Maybe (m Node)

-- orResolver :: Monad m => Resolver m
-- orResolver cont node@(Sequence tag nodes)
--   | tag == mkTag "!or" = Just $ pure $ head (cont <$> nodes)
--   | otherwise = Nothing

baseResolver :: Monad m => Resolver m
baseResolver cont node@(Scalar _) = Just $ pure node
baseResolver cont (Mapping tag mapping) = Just $ Mapping tag <$> cont `mapM` mapping
baseResolver cont (Sequence tag nodes) = Just $ Sequence tag <$> cont `mapM` nodes
baseResolver _ node = Just $ pure node

resolve :: MonadThrow m => Resolver m -> Node -> m Node
resolve res node = fromMaybe
  (throwM $ ParseError "Failed to parse")
  (res (resolve res) node)
