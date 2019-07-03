{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiWayIf #-}
module Data.Config.Internals where

import           Control.Monad.Catch       (Exception, MonadThrow, throwM)
import           Control.Monad.Except
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.ByteString.Lazy      (ByteString)
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.YAML
import           Data.YAML.Event           (Tag, mkTag)

newtype ParseError = ParseError String deriving (Eq, Show)

instance Exception ParseError


readConfig :: (MonadIO m, FromYAML v) => FilePath -> ErrT m v
readConfig = undefined

decodeConfig :: (Monad m, MonadIO m, MonadThrow m, FromYAML v) => ByteString -> m v
decodeConfig bs = case decodeNode bs of
  Left description -> throwM $ ParseError description
  Right [Doc v] -> do
    node' <- runExceptT (resolve baseResolver v)
    -- Make this more concise: abstract over something like Either String a -> (MonadThrow m => m a)
    case node' of
      Left err -> throwM $ ParseError err
      Right node ->
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

data ResultT m a
  = Resolved (m (Maybe a))
  | NoMatch
  deriving (Functor)


defaultResolver :: Monad m => Resolver m
defaultResolver = orResolver <> baseResolver

type ErrT m a = ExceptT String m a

newtype Resolver m =
  Resolver ((Node -> ErrT m Node) -> Node -> Maybe (ErrT m Node))

-- envResolver :: MonadIO m => Resolver m
-- envResolver cont node@(Scalar (SUnknown tag text))
--   | if tag == "!env" =

orResolver :: Monad m => Resolver m
orResolver = Resolver orResolver'
  where

    orResolver' cont node@(Sequence tag nodes) =
      if | tag == mkTag "!or" -> Just . ExceptT $ do
             results <- (runExceptT . cont) `mapM` nodes
             case firstSuccess results of
               Nothing -> pure $ Left "No successful resolver for !or tag"
               Just a  -> pure $ Right a
         | otherwise -> Nothing
    orResolver' _ node = Nothing

    firstSuccess xs = case dropWhile isLeft xs of
      []          -> Nothing
      (Right x:_) -> Just x

instance Semigroup (Resolver m) where
  (Resolver f) <> (Resolver g) = Resolver $ \cont node ->
    case f cont node of
      Just n  -> Just n
      Nothing -> g cont node

baseResolver :: Monad m => Resolver m
baseResolver = Resolver baseResolver'
  where

    baseResolver' cont node@(Scalar _) = Just (pure node)
    baseResolver' cont (Mapping tag mapping) =
      Just (Mapping tag <$> cont `mapM` mapping)
    baseResolver' cont (Sequence tag nodes) =
      Just (Sequence tag <$> cont `mapM` nodes)
    baseResolver' _ node = Just (pure node)

resolve :: MonadThrow m => Resolver m -> Node -> ErrT m Node
resolve res@(Resolver f) node = fromMaybe error (f (resolve res) node)
  where
    error = throwError "Failed to resolve"
