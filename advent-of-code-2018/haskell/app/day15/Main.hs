{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Prelude          hiding (readFile)

import           Data.Either
import           Data.Function
import           Data.List
import qualified Data.Set as Set
import           Data.Semigroup
import           Data.Set         (Set)
import           Data.Vector      (Vector, (!), (//))
import qualified Data.Vector      as Vector
import           System.IO.Strict (readFile)

import           Debug.Trace

data Race = Elf | Goblin deriving (Eq, Show)

data Creature = Creature
  { crRace      :: !Race
  , crHitpoints :: !Int
  , crAttack    :: !Int
  , crSymbol    :: !Char
  } deriving (Eq, Show)

data Point = Point Int Int deriving (Eq)

instance Ord Point where
  (Point r c) `compare` (Point r' c') =
    let cmp = r `compare` r'
    in if cmp == EQ then c `compare` c' else cmp

instance Show Point where
  show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Semigroup Point where
  Point x y <> Point u v = Point (x + u) (y + v)

data Square
  = WithCreature Creature
  | Empty
  | Wall
  deriving (Eq, Show)

toChar :: Square -> Char
toChar (WithCreature creature) = crSymbol creature
toChar Empty = '.'
toChar Wall = '#'

hasRace :: Caves -> Race -> Point -> Bool
hasRace caves race point =
  case caves <!> point of
    (WithCreature creature) -> crRace creature == race
    _ -> False

-- |Data structure that holds the caves. Should be fairly fast to modify squares.
data Caves = Caves
  { squares :: !(Vector Square)
  , width   :: !Int } deriving (Eq)

instance Show Caves where
  show caves@(Caves{..}) =
    (squares &
    fmap toChar &
    chunkVector width &
    fmap show &
    unlines) -- ++ showCreatures
    where
      chunkVector n xs
        | Vector.null xs = []
        | otherwise = let (segment, remains) = Vector.splitAt n xs
                      in (segment:chunkVector n remains)
      showCreatures =
        let creatures = findCreatures caves
        in show $ (\p -> (p, displayCreature $ caves <!> p)) <$> creatures
      displayCreature (WithCreature creature)
        | crRace creature == Elf = "E(" ++ show (crHitpoints creature) ++ ")"
        | crRace creature == Goblin = "G(" ++ show (crHitpoints creature) ++ ")"

index :: Int -> Point -> Int
index width (Point r c) = (r * width + c)

toPoint :: Int -> Int -> Point
toPoint width i = uncurry Point $ quotRem i width

exchangeSquares :: Caves -> Point -> Point -> Caves
exchangeSquares Caves{..} p q =
  let squares' = squares //
        [ (index width p, squares ! index width q)
        , (index width q, squares ! index width p)]
  in Caves squares' width

putSquare :: Caves -> Point -> Square -> Caves
putSquare Caves{..} p sq =
  let squares' = squares // [ (index width p, sq)]
  in Caves squares' width

-- |Get the square in the caves at the specified point.
(<!>) :: Caves -> Point -> Square
(<!>) Caves{..} point =
  squares ! index width point

-- |Helper function to find the unique value in a list, if it exists.
unique :: Eq a => [a] -> Either String a
unique [] = Left "empty list"
unique (x:xs) = recur x xs
  where
    recur u [] = Right u
    recur u (x:xs) = if u == x
      then recur u xs else Left "multiple distinct characters"

-- TODO: make this into a Read instance
-- |Attempt to parse the string represenation of the cave into an actual cave
-- object.
parseCaves :: String -> Either String Caves
parseCaves s =
  let rows = lines s
      width = unique (length `fmap` rows)
      makeElf = Creature Elf 200 3 'E'
      makeGoblin = Creature Goblin 200 3 'G'
      parseSquare c
        | c == '.' = Right Empty
        | c == '#' = Right Wall
        | c == 'E' = Right $ WithCreature makeElf
        | c == 'G' = Right $ WithCreature makeGoblin
        | otherwise = Left $ [c] ++ " does not represent a valid square"
      squares :: Either String [Square]
      squares = parseSquare `mapM` concat rows
  in Caves <$> (Vector.fromList <$> squares) <*> width

-- |Convenience function the read the caves from a file on disk.
readCaves :: FilePath -> IO (Either String Caves)
readCaves path = parseCaves <$> readFile path

data RoseTree a = RoseTree a [RoseTree a] deriving (Show)

-- TODO: It seems that visited is only passed along a single branch
-- |Create a tree where the first node values of the branches of a node with
-- value 'x' are 'gen x'.
spawnTree :: Ord a => (Set a -> a -> [a]) -> Set a -> a -> RoseTree a
spawnTree gen visited start = RoseTree start
  (spawnTree gen (Set.insert start visited) `fmap` gen visited start)

-- |Do a depth-first search through 'tree' and return the shortest paths
-- leading from the root to a node that satisfied 'pred'.
shortestBranches :: Show a => (a -> Bool) -> RoseTree a -> [[a]]
shortestBranches pred tree = descend [([], tree)]
  where
    descend [] = []
    descend ts =
      let found = filter (pred . value . snd) ts
      in if null found
         then descend $ do
           (acc, RoseTree v branches) <- ts
           branch <- branches
           pure $ (v:acc, branch)
         else [reverse (v:acc) | (acc, RoseTree v _) <- found]
    value (RoseTree v _) = v

shortestPaths :: (Ord a, Show a) => a -> (a -> [a]) -> (a -> Bool) -> [[a]]
shortestPaths start gen pred = descend (Set.fromList [start]) [[start]]
  where
    descend !visited [] = []
    descend !visited !paths =
      let matches = filter (pred . head) paths
          visited' = foldl' (flip Set.insert) visited (head <$> paths)
      in if null matches
         then descend visited' $ do
           path <- paths
           next <- gen (head path)
           if next `elem` visited' then [] else pure (next:path)
         else reverse <$> matches


-- |The points that are potentially reachable from 'point' with a single move.
neighborhood :: Point -> [Point]
neighborhood point = ((point <>) `fmap` directions)
  where directions = [Point (-1) 0, Point 0 (-1), Point 0 1, Point 1 0]

-- |Compute the shortest paths through empty squares leading to a square that
-- satisfies 'condition', starting from 'start'.
shortestPathsToSquare :: Caves -> Point -> (Square -> Bool) -> [[Point]]
shortestPathsToSquare caves start condition =
  shortestPaths start gen (\p -> condition $ caves <!> p)
  where
    gen p =
      let valid q = (caves <!> q == Empty || condition (caves <!> q))
      in valid `filter` neighborhood p

-- |Return the shortest path to an enemy, if the square at 'point' has a
-- creature. Otherwise, return Nothing. This function is responsible for
-- returning the paths in the right order.
shortestPathsToEnemy :: Caves -> Point -> [[Point]]
shortestPathsToEnemy caves point =
  case caves <!> point of
    WithCreature creature ->
      shortestPathsToSquare caves point (hasEnemy creature)
    _ -> []
  where
    hasEnemy creature (WithCreature onSquare) =
      crRace creature /= crRace onSquare
    hasEnemy _ _ = False

data Action
  = Move Point Point
  | Attack Point Point
  deriving (Show)

-- |Determines the action to be executed for the point. Returns Nothing when the
-- square does not contain a creature, or when there is nothing to do for the
-- creature.
getActions :: Caves -> Point -> [Action]
getActions caves point = do
  case shortestPathsToEnemy caves point of
    -- No more enemies reachable
    [] -> []

    -- Enemy already in reach: no need to move
    ([_, q]:rest) ->
      [Attack point (selectWeakest (q:last `fmap` rest))]

    -- Enemy within reach after moving: attack immediately
    ([_, q, r]:rest) ->
      let enemies = filter (\(_:q':_) -> q == q') rest
      in
      [Move point q, Attack q (selectWeakest (r:last `fmap` enemies))]

    ((_:q:_):_) -> [Move point q]
  where
    selectWeakest (p:ps) = foldl' leastHitpoints p ps
    leastHitpoints p q =
      let WithCreature c = caves <!> p
          WithCreature d = caves <!> q
      in if crHitpoints c <= crHitpoints d then p else q

-- |Updates the state in 'caves' with the result of executing the action.
executeAction :: Caves -> Action -> Caves
executeAction caves (Move p q) = exchangeSquares caves p q
executeAction caves (Attack p q) =
  let WithCreature attacker = caves <!> p
      WithCreature defender = caves <!> q
      defender' = defender { crHitpoints = crHitpoints defender - crAttack attacker }
  in if crHitpoints defender' < 0
     then putSquare caves q Empty
     else putSquare caves q $ WithCreature defender'

-- |Find all creatures in the caves.
findCreatures :: Caves -> [Point]
findCreatures (Caves {..}) =
  let indices = flip Vector.findIndices squares $ \case
        WithCreature _ -> True
        _ -> False
  in Vector.toList $ toPoint width `fmap` indices

-- |Execute a round, in which every creature in the cave takes an action.
executeRound :: Caves -> Caves
executeRound caves =
  foldl' step caves $ findCreatures caves
  where
      step caves' point = trace (show point ++ " ") $
        foldl' executeAction caves'  (getActions caves' point)

-- |Execute rounds until no unit can act anymore.
executeBattle :: Caves -> (Int, Caves)
executeBattle caves = recur 0 caves
  where
    recur i caves' =
      let caves'' = executeRound caves'
      in if caves' == caves'' then (i - 1, caves') else recur (i + 1) caves''

executeBattle' :: Caves -> [Caves]
executeBattle' caves = caves:executeBattle' (executeRound caves)

executeBattleIO :: Caves -> IO (Int, Caves)
executeBattleIO caves = recur 0 caves
  where
    recur i caves' = do
      putStrLn $ "==== ROUND: " ++ show i ++ "===="
      let caves'' = executeRound caves'
      if caves' == caves''
      then pure (i - 1, caves')
      else recur (i + 1) caves''

executeRoundUpTo :: Int -> Caves -> [Caves]
executeRoundUpTo n caves = take n (scanl step caves creatures)
  where creatures = findCreatures caves
        step caves' point =
          foldl' executeAction caves'  (getActions caves' point)

-- |Get the state of the battle after 'n' round, starting from 'caves'.
getRound :: Int -> Caves -> Caves
getRound n caves
  | n == 0 = caves
  | otherwise = getRound (n - 1) $ executeRound caves

answer15a :: Caves -> (Int, Int)
answer15a caves =
  let (rounds, state) = executeBattle caves
  in (rounds, sumHitpoints state)
  where

sumHitpoints :: Caves -> Int
sumHitpoints caves =
  let points = findCreatures caves
      hp (WithCreature creature) = crHitpoints creature
  in foldl' (+) 0 $ (hp . (caves <!>)) <$> points


getRight :: Either a b -> b
getRight (Right b) = b

caves0 :: Caves
caves0 = getRight $ parseCaves $ unlines
  [ "#########"
  , "#G..G..G#"
  , "#.......#"
  , "#.......#"
  , "#G..E..G#"
  , "#.......#"
  , "#.......#"
  , "#G..G..G#"
  , "#########"
  ]

caves1 :: Caves
caves1 = getRight $ parseCaves $ unlines
  [ "#######"
  , "#.G...#"
  , "#...EG#"
  , "#.#.#G#"
  , "#..G#E#"
  , "#.....#"
  , "#######"
  ]

caves2 :: Caves
caves2 = getRight $ parseCaves $ unlines
  [ "#######"
  , "#G..#E#"
  , "#E#E.E#"
  , "#G.##.#"
  , "#...#E#"
  , "#...E.#"
  , "#######"
  ]

caves3 :: Caves
caves3 = getRight $ parseCaves $ unlines
  [ "#######"
  , "#E..EG#"
  , "#.#G.E#"
  , "#E.##E#"
  , "#G..#.#"
  , "#..E#.#"
  , "#######"
  ]

caves4 :: Caves
caves4 = getRight $ parseCaves $ unlines
  [ "#######"
  , "#.E...#"
  , "#.#..G#"
  , "#.###.#"
  , "#E#G#G#"
  , "#...#G#"
  , "#######"
  ]

caves5 :: Caves
caves5 = getRight $ parseCaves $ unlines
  [ "#########"
  , "#G......#"
  , "#.E.#...#"
  , "#..##..G#"
  , "#...##..#"
  , "#...#...#"
  , "#.G...G.#"
  , "#.....G.#"
  , "#########"
  ]


caves :: IO Caves
caves = getRight <$> readCaves "res/input-15.txt"

c = last $ executeRoundUpTo 9 (getRound 2 caves0)

p = shortestPathsToEnemy c (Point 5 7)


p' = take 1 `fmap` p

db = getRound 15 . getRight <$> readCaves "res/input-15.txt"

main :: IO ()
main = do
  caves <- getRight <$> readCaves "res/input-15.txt"
  let caves' = getRound 18 caves
  putStrLn $ show caves'
