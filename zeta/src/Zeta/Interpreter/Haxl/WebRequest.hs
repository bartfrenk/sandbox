module Zeta.Interpreter.Haxl.WebRequest where

import           Control.Monad.Except
import           Data.Aeson                      (eitherDecode)
import           Data.Hashable
import           Data.Map.Strict                 (Map, (!?))
import qualified Data.Map.Strict                 as Map
import           Data.Set                        (Set)
import qualified Data.Text                       as T
import           Data.Validation                 (Validation (..))
import           Data.Yaml
import           Haxl.Core
import           Network.HTTP.Client
import qualified Debug.Trace as Debug
import           Control.Monad.Writer

import           Zeta.Interpreter                hiding (execute)
import qualified Zeta.Interpreter                as I
import           Zeta.Interpreter.Haxl.Resources
import qualified Zeta.Interpreter.Types          as I
import           Zeta.Syntax
import           Zeta.Types

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
  { fetches :: Map (URN, Set Name) (Resource Value)
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
        case fetches env !? (urn, Map.keysSet args) of
          Nothing -> putSuccess var Nothing
          Just resource -> case selectors resource !? urn of
            Nothing -> putSuccess var Nothing
            Just select -> do
              result <- fetchResource (manager state) resource args
              case result of
                Right val -> putSuccess var (select `from` val)
                Left _ -> putSuccess var Nothing

data FetchFailure
  = InvalidTemplateStringArgs [Name]
  | FailedToDecodeJSON String

-- |This function is called from the 'GenHaxl' monad to make the actual web
-- request and decode the results into a JSON value.
fetchResource :: Manager -> Resource Value -> Map Name Literal -> IO (Either FetchFailure Value)
fetchResource manager Resource{makeUri} args =
  case makeUri args of
    Failure err -> pure $ Left $ InvalidTemplateStringArgs err
    Success uri -> do
      print $ unUri uri
      request <- parseRequest $ T.unpack $ unUri uri
      body <- responseBody <$> httpLbs request manager
      case eitherDecode body of
        Left err -> pure $ Left $ FailedToDecodeJSON err
        Right val -> pure $ Right val

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

test1 expr = do
  Right ipstackDesc <- decodeFileEither "docs/resource.yaml"
  let ipstack = makeResource ipstackDesc
  let fetches = makeFetches ipstack
  execute
    [ ((["city"], []), Fetch ["city"] [("ip-address", "82.171.74.76")])
    , ((["country_code"], []), Fetch ["country_code"] [("ip-address", "82.171.74.76")])
    ] (WebRequestEnv fetches) expr

test2 expr = do
  fetches <- loadFetches "docs/resources.yaml"
  execute
    [ ((["city"], []), Fetch ["city"] [("ip_address", "82.171.74.76")])
    , ((["country_code"], []), Fetch ["country_code"] [("ip_address", "82.171.74.76")])
    , ((["wind_degree"], []), Fetch ["wind_degree"]
        [ ("city", App (External ["city"]) [])
        , ("country_code", App (External ["country_code"]) [])])]
    (WebRequestEnv fetches) expr


-- test3 = do
--   Right ipstack <- decodeFileEither "docs/resource.yaml"
--   print (url ipstack [("ip-address", "8.8.8.8")])

x :: IO (Either RuntimeError Expr, Assignments)
x = test2 (Assignment "x" (Literal $ I 0))




u :: IO (Either RuntimeError Expr, Assignments)
u = test2 (Fetch ["is_eu"] [("ip-address", "82.171.74.76")])


v :: IO (Either RuntimeError Expr, Assignments)
v = test2 (Fetch ["country_code"] [("ip-address", "82.171.74.76")])

w :: IO (Either RuntimeError Expr, Assignments)
w = test2 (Fetch ["wind_degree"] [("city", "Eindhoven"), ("country-code", "nl")])


a :: IO (Either RuntimeError Expr, Assignments)
a = test2 (App (External ["country_code"]) [])


b :: IO (Either RuntimeError Expr, Assignments)
b = test2 (App (External ["wind_degree"]) [])
