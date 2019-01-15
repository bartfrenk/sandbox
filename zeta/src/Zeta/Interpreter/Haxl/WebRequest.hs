module Zeta.Interpreter.Haxl.WebRequest where

import           Data.Aeson.Lens
import           Data.ByteString.Lazy
import           Data.Hashable
import           Data.Map.Strict        (Map, (!), (!?))
import qualified Data.Map.Strict        as Map
import           Haxl.Core
import           Lens.Micro.Extras
import           Network.HTTP.Client

import           Zeta.Interpreter       hiding (execute)
import qualified Zeta.Interpreter       as I
import qualified Zeta.Interpreter.Types as I
import           Zeta.Syntax

import           Control.Monad.Writer


type URL = String

data WebRequest a where
  WebRequest :: URN -> Map Name Literal -> WebRequest (Maybe Expr)

deriving instance Eq (WebRequest a)
deriving instance Show (WebRequest a)

instance Hashable (WebRequest a) where
  hashWithSalt s (WebRequest urn args) = hashWithSalt s (urn, Map.toList args)

ipstack :: Resource
ipstack = Resource
  { url = const "http://api.ipstack.com/82.171.74.76?access_key=2b5b300141b464d17f33178463bc2456"
  , contexts = [(["city"],  fmap (Literal . S) . preview (key "city" . _String))]
  }

instance ShowP WebRequest where
  showp = show

instance DataSourceName WebRequest where
  dataSourceName _ = "WebRequest"

type Selector = ByteString -> Maybe Expr

data Resource = Resource
  { url      :: Map Name Literal -> URL
  , contexts :: Map URN Selector
  }

newtype WebRequestEnv = WebRequestEnv
  { fetches :: Map (URN, Map Name Literal) Resource
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
        case fetches env !? (urn, args) of
          Nothing -> putSuccess var Nothing
          Just resource -> case contexts resource !? urn of
            Nothing -> putSuccess var Nothing
            Just selector -> do
              result <- fetchResource (manager state) resource args
              putSuccess var (selector result)

fetchResource :: Manager -> Resource -> Map Name Literal -> IO ByteString
fetchResource manager Resource{url} args = do
  request <- parseRequest $ url args
  responseBody <$> httpLbs request manager

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


test = do
  mgr <- newManager defaultManagerSettings
  resource <- fetchResource mgr ipstack []
  pure $ (contexts ipstack ! ["city"]) resource

test2 expr = do
  execute [] (WebRequestEnv [((["city"], []), ipstack)]) expr


x :: IO (Either RuntimeError Expr, Assignments)
x = test2 (Assignment "x" (Literal $ I 0))


y :: IO (Either RuntimeError Expr, Assignments)
y = test2 (Fetch ["city"] [])
