module Zeta.Interpreter.Haxl.WebRequest where

import           Data.Aeson
import           Data.ByteString.Lazy
import           Data.Hashable
import           Data.Map.Strict                 (Map, (!), (!?))
import qualified Data.Map.Strict                 as Map
import           Data.Set                        (Set)
import           Data.Yaml
import           Haxl.Core
import           Lens.Micro.Extras
import           Network.HTTP.Client

import           Control.Monad.Writer
import qualified Debug.Trace                     as D

import           Zeta.Interpreter                hiding (execute)
import qualified Zeta.Interpreter                as I
import           Zeta.Interpreter.Haxl.Resources
import qualified Zeta.Interpreter.Types          as I
import           Zeta.Syntax

data WebRequest a where
  WebRequest :: URN -> Map Name Literal -> WebRequest (Maybe Expr)

deriving instance Eq (WebRequest a)
deriving instance Show (WebRequest a)

instance Hashable (WebRequest a) where
  hashWithSalt s (WebRequest urn args) = hashWithSalt s (urn, Map.toList args)

instance ShowP WebRequest where
  showp = show

instance DataSourceName WebRequest where
  dataSourceName _ = "WebRequest"

newtype WebRequestEnv = WebRequestEnv
  { fetches :: Map (URN, Set Name) Resource
  }

instance StateKey WebRequest where
  data State WebRequest = WebRequestState
    { manager :: Manager }

-- TODO: probably best to make finer-grained distinctions between
-- different fetch results.
instance DataSource WebRequestEnv WebRequest where
  fetch state _flags env blockedFetches = SyncFetch $
    mapM_ processFetch blockedFetches

    where

      processFetch :: BlockedFetch WebRequest -> IO ()
      processFetch (BlockedFetch (WebRequest urn args) var) =
        D.trace (show (urn, args)) $
        case fetches env !? (urn, Map.keysSet args) of
          Nothing -> putSuccess var Nothing
          Just resource -> case contexts resource !? urn of
            Nothing -> putSuccess var Nothing
            Just selector -> do
              result <- fetchResource (manager state) resource args
              case result of
                Right val -> D.trace (show val) $ putSuccess var (selector val)
                Left _ -> putSuccess var Nothing

fetchResource :: Manager -> Resource -> Map Name Literal -> IO (Either String Value)
fetchResource manager Resource{url} args = D.trace (show (url args)) $ do
  request <- parseRequest $ url args
  body <- responseBody <$> httpLbs request manager
  pure $ eitherDecode body

-- ==== TEST (remove later)

runtimeEnv :: MonadTrans t
           => Map I.ExtSignature Expr
           -> RuntimeEnv (t (GenHaxl WebRequestEnv))
runtimeEnv externals = RuntimeEnv
  { _externals = externals
  , _fetch = \urn args -> lift $ dataFetch (WebRequest urn args)
  }


newtype Assignments = Assignments (Map Name Expr) deriving (Show)

instance Monoid Assignments where
  mempty = Assignments mempty
  (Assignments m1) `mappend` (Assignments m2) =
    Assignments $ m1 `Map.union` m2

type MockM = WriterT Assignments (GenHaxl WebRequestEnv)

instance Domain MockM where
  assign name expr = tell $ Assignments [(name, expr)]


execute :: MonadIO m =>
     Map I.ExtSignature Expr
  -> WebRequestEnv
  -> Expr
  -> m (Either RuntimeError Expr, Assignments)
execute externals env expr = liftIO $ do
  manager <- newManager defaultManagerSettings
  let stateStore = stateSet (WebRequestState manager) stateEmpty

  let rtEnv = runtimeEnv externals
  haxlEnv <- initEnv stateStore env
  runHaxl haxlEnv $ runWriterT (I.execute rtEnv expr)


-- test = do
--   mgr <- newManager defaultManagerSettings
--   resource <- fetchResource mgr ipstack []
--   pure $ (contexts ipstack ! ["city"]) resource

test2 expr = do
  Right ipstack <- decodeFileEither "docs/resource.yaml"
  execute [] (WebRequestEnv
              [ ((["city"], ["ip-address"]), ipstack)
              , ((["country"], ["ip-address"]), ipstack)
              , ((["is_eu"], ["ip-address"]), ipstack)
              ]) expr

test3 = do
  Right ipstack <- decodeFileEither "docs/resource.yaml"
  print (url ipstack [("ip-address", "8.8.8.8")])

x :: IO (Either RuntimeError Expr, Assignments)
x = test2 (Assignment "x" (Literal $ I 0))




y :: IO (Either RuntimeError Expr, Assignments)
y = test2 (Fetch ["is_eu"] [("ip-address", "82.171.74.76")])
