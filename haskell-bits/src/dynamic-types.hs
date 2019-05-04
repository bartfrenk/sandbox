{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

import Data.Dynamic
import Control.Concurrent.Chan
import Type.Reflection (TypeRep, typeRep)

-- Simple dynamic typing

newtype ObservationBatch = ObservationBatch [Dynamic] deriving (Show)

data BernoulliObsType = Win | Seen deriving (Show)

data BernoulliObs = BernoulliObs
  { obsType :: BernoulliObsType
  , agentId :: Int
  , action :: String
  } deriving (Show)

data LinearObs = LinearObs
  { obsType :: Double
  , agentId :: Int
  , action :: String
  } deriving (Show)

batch :: ObservationBatch
batch = ObservationBatch
  [ toDyn (BernoulliObs Win 1 "A")
  , toDyn (LinearObs 1.0 2 "B")
  ]


putToChan :: IO ()
putToChan = do
  chan <- newChan
  writeChan chan (toDyn (1 :: Int))
  writeChan chan (toDyn ("hello" :: String))

  fromDynamic <$> readChan chan >>= \case
    Nothing -> pure ()
    Just val -> print (val :: Int)
  fromDynamic <$> readChan chan >>= \case
    Nothing -> pure ()
    Just val -> print (val :: Int)



class HasAgentId a where
  agentId :: a -> Int

data Observation where
  Obs :: HasAgentId a => TypeRep a -> a -> Observation

instance HasAgentId Observation where
  agentId (Obs _ x) = agentId x

toObservation :: (Typeable a, HasAgentId a) => a -> Observation
toObservation x = Obs typeRep x

instance HasAgentId BernoulliObs where
  agentId = agentId

x :: Observation
x = toObservation $ BernoulliObs Win 1 "A"


data AgentRPM act = AgentRPM act
  { dist :: Map act Beta
  }





data AgentRepository id m = AgentRepository
  { getAgent :: id -> m 

  }
