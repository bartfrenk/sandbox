module Zeta.Interpreter.Haxl where

import Control.Monad.Writer
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map

import Data.Hashable
import Haxl.Core

import Zeta.Syntax
import Zeta.Interpreter
import qualified Zeta.Interpreter.Types as I


type UserEnv = String

data LocalSource a where
  LocalValue :: URN -> Map Name Literal -> LocalSource Expr

data TestSource a where
  TestSource :: String -> TestSource String

deriving instance Eq (TestSource a)

deriving instance Show (TestSource a)

instance Hashable (TestSource a) where
  hashWithSalt s (TestSource t) = hashWithSalt s t

instance ShowP TestSource where
  showp = show

instance DataSourceName TestSource where
  dataSourceName _ = "TestSource"

instance StateKey TestSource where
  data State TestSource = UserState {}



instance DataSource UserEnv TestSource where
  fetch _state _flags userEnv blockedFetches = SyncFetch $
    mapM_ processFetch blockedFetches

      where
        processFetch :: BlockedFetch TestSource -> IO ()
        processFetch (BlockedFetch (TestSource s) var) =
          putSuccess var $ unwords [userEnv, s]


test = do
  let stateStore = stateSet (UserState {}) stateEmpty
  env <- initEnv stateStore "hello"
  runHaxl env x
  where
    x :: GenHaxl UserEnv String
    x = dataFetch (TestSource "world!")

{-
type Request req a =
  ( Eq (req a)
  , Hashable (req a)
  , Typeable (req a)
  , Show (req a)
  , Show a
  )
-}

{-
class (DataSourceName req, StateKey req, ShowP req) => DataSource u req
-}

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

mockExecute :: Map I.ExtSignature Expr
            -> MockRequestEnv
            -> Expr
            -> IO (Either RuntimeError Expr, Assignments)
mockExecute externals env expr = do
  let runtimeEnv = mockRuntimeEnv externals
  let stateStore = stateSet MockRequestState stateEmpty
  haxlEnv <- initEnv stateStore env
  runHaxl haxlEnv $ runWriterT (execute runtimeEnv expr)


