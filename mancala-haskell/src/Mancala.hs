{-# LANGUAGE RecordWildCards #-}
module Mancala where

data Player = P1 | P2 deriving (Eq, Show)

data GameState = GameState
  { pits :: [Int] -- ^ Rotating list with the pits
  , top :: Int -- ^ The pit number of the head of pits
  , scores :: (Int, Int) -- ^ Number of stones in the stores
  , turn :: Player -- ^ The player currently making the move
  } deriving (Show)

putStone :: GameState -> GameState
putStone st@GameState{..} = st { pits = (head pits + 1):tail pits }

incScore :: Int -> GameState -> GameState
incScore n st =
  let (p1, p2) = scores st
  in case turn st of
    P1 -> st { scores = (p1 + n, p2) }
    P2 -> st { scores = (p1, p2 + n) }

toggleTurn :: GameState -> GameState
toggleTurn st =
  case turn st of
    P1 -> st { turn = P2 }
    P2 -> st { turn = P1 }

step :: Int -> GameState -> GameState
step n st@GameState {..}
  | n == 0 = st
  | n > 1 =
    -- TODO: This is slow due to list concatenation
    let st' = st { pits = tail pits ++ [head pits]
                 , top = top + 1 `mod` length pits
                 }
    in step (n - 1) st'

initialState = GameState
  { pits = side ++ side
  , top = 0
  , scores = (0, 0)
  , turn = P1
  }
  where side = 0:replicate 6 4

capture :: GameState -> GameState
capture st =
  let st'@GameState{..} = opposite st
      stones = head pits
  in opposite $ incScore stones $ st' { pits = 0:tail pits }

opposite :: GameState -> GameState
opposite st@GameState{..} = step (length pits + 1 - 2 * top) st

deposit :: Int -> GameState -> GameState
deposit n st@GameState{..}
  | turn == P1 && n > 2 && top == 6 =
      deposit (n - 2) (step 1 $ incScore 1 $ putStone st)
  | turn == P2 && n > 2 && top == 12 =
      deposit (n - 2) (step 1 $ incScore 1 $ putStone st)
  | n == 0 = st
  | n == 1 =
      putStone $ toggleTurn $ if head pits == 0 then capture st else st
  | n > 1 =
      deposit (n - 1) (step 1 $ putStone st)
