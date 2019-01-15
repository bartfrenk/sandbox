module Zeta.Interpreter.MockRequest where

import           Control.Monad.Writer
import           Data.Map.Strict        (Map, (!?))
import qualified Data.Map.Strict        as Map

import           Data.Hashable
import           Haxl.Core

import           Zeta.Interpreter       hiding (execute)
import qualified Zeta.Interpreter       as I
import qualified Zeta.Interpreter.Types as I
import           Zeta.Syntax

-- |Request to fetch expressions from an in memory-lookup table
data MockRequest a where
  MockRequest :: URN -> Map Name Literal -> MockRequest (Maybe Expr)

deriving instance Eq (MockRequest a)
deriving instance Show (MockRequest a)

instance Hashable (MockRequest a) where
  hashWithSalt s (MockRequest urn args) = hashWithSalt s (urn, Map.toList args)

instance ShowP MockRequest where
  showp = show

instance DataSourceName MockRequest where
  dataSourceName _ = "MockRequest"

instance StateKey MockRequest where
  data State MockRequest = MockRequestState

type MockRequestEnv = Map (URN, Map Name Literal) Expr

instance DataSource MockRequestEnv MockRequest where
  fetch _state _flags env blockedFetches = SyncFetch $
    mapM_ processFetch blockedFetches

    where
      processFetch :: BlockedFetch MockRequest -> IO ()
      processFetch (BlockedFetch (MockRequest urn args) var) =
        case env !? (urn, args) of
          Just expr -> putSuccess var (Just expr)
          Nothing -> putSuccess var Nothing

mockRuntimeEnv :: MonadTrans t
               => Map I.ExtSignature Expr
               -> RuntimeEnv (t (GenHaxl MockRequestEnv))
mockRuntimeEnv externals = RuntimeEnv
  { _externals = externals
  , _fetch = \urn args -> lift $ dataFetch (MockRequest urn args)
  }

newtype Assignments = Assignments (Map Name Expr)

instance Monoid Assignments where
  mempty = Assignments mempty
  (Assignments m1) `mappend` (Assignments m2) =
    Assignments $ m1 `Map.union` m2

type MockM = WriterT Assignments (GenHaxl MockRequestEnv)

instance Domain MockM where
  assign name expr = tell $ Assignments [(name, expr)]

-- |Executes the interpreter on an expression with a prespecified lookup table
-- of data fetches. Uses Haxl for the data fetches.
execute :: MonadIO m =>
     Map I.ExtSignature Expr -- ^ The external functions available to the
                             -- interpreter.
  -> MockRequestEnv          -- ^ A lookup table of data fetches, keyed by the
                             -- signature of the fetch.
  -> Expr                    -- ^ The expression to evaluate.
  -> m (Either RuntimeError Expr, Assignments)
execute externals env expr = liftIO $ do
  let runtimeEnv = mockRuntimeEnv externals
  let stateStore = stateSet MockRequestState stateEmpty
  haxlEnv <- initEnv stateStore env
  runHaxl haxlEnv $ runWriterT (I.execute runtimeEnv expr)
