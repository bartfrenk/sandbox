{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Data.Config.Internals where

import           Control.Monad.Catch       (Exception, MonadThrow, throwM)
import           Control.Monad.Except
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.ByteString.Lazy      (ByteString)
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.String.Conv
import           Data.YAML                 (Doc (..), FromYAML, Node (..),
                                            Scalar (..))
import qualified Data.YAML                 as YAML
import           Data.YAML.Event           (Tag, mkTag)
import           System.Environment        (lookupEnv)

newtype ParseError = ParseError String deriving (Eq, Show)

instance Exception ParseError

readConfig :: (MonadIO m, FromYAML v) => FilePath -> ErrT m v
readConfig = undefined

decodeConfig :: (Monad m, MonadIO m, MonadThrow m, FromYAML v)
             => Resolver m -> ByteString -> m v
decodeConfig res bs = fromErrT (decodeNode bs >>= resolve res >>= fromNode)

  where
    fromErrT act = runExceptT act >>= \case
      Left err -> throwM $ ParseError err
      Right v -> pure v

test :: IO Node
test = decodeConfig (envResolver <> orResolver <> baseResolver) example
  where
    example = "host: !or [!env HOST, localhost]\n\
              \port: 123\n\
              \database:\n\
              \  password: secret\n\
              \password: !secret password"

defaultResolver :: MonadIO m => Resolver m
defaultResolver = orResolver <> envResolver <> baseResolver

type ErrT m a = ExceptT String m a

newtype Resolver m =
  Resolver ((Node -> ErrT m Node) -> Node -> Maybe (ErrT m Node))

instance Semigroup (Resolver m) where
  (Resolver f) <> (Resolver g) = Resolver $ \cont node ->
    case f cont node of
      Just n  -> Just n
      Nothing -> g cont node

decodeNode :: (Monad m, StringConv s ByteString) => s -> ErrT m Node
decodeNode s = case YAML.decodeNode (toS s) of
  Left err         -> throwError err
  Right [Doc node] -> pure node
  _                -> throwError "Multiple YAML not supported"

fromNode :: (Monad m, FromYAML v) => Node -> ErrT m v
fromNode node = case YAML.parseEither $ YAML.parseYAML node of
  Left err -> throwError err
  Right v  -> pure v

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

envResolver :: MonadIO m => Resolver m
envResolver = Resolver envResolver'
  where
    envResolver' cont node@(Scalar (SUnknown tag text)) =
      if | tag == mkTag "!env" -> Just $
             liftIO (lookupEnv (toS text)) >>= \case
               Nothing -> throwError "Could not find env"
               Just s -> decodeNode s >>= cont
         | otherwise -> Nothing
    envResolver' _ _ = Nothing

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
