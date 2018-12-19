module Day15 where

import           Prelude          hiding (readFile)

import           Data.Either
import           Data.Function
import           Data.List
import           Data.Semigroup
import           Data.Vector      (Vector, (!))
import qualified Data.Vector      as Vector
import           System.IO.Strict (readFile)

data Race = Elf | Goblin deriving (Eq, Show)

data Point = Point Int Int

instance Show Point where
  show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Semigroup Point where
  Point x y <> Point u v = Point (x + u) (y + v)

data Square
  = Creature Race
  | Empty
  | Wall
  deriving (Eq, Show)

toChar :: Square -> Char
toChar (Creature Elf) = 'E'
toChar (Creature Goblin) = 'G'
toChar Empty = '.'
toChar Wall = '#'


hasRace :: Caves -> Race -> Point -> Bool
hasRace caves race point =
  case caves <!> point of
    Creature race' -> race == race
    _ -> False

data Caves = Caves
  { squares :: Vector Square
  , width   :: Int }

chunkVector :: Int -> Vector a -> [Vector a]
chunkVector n xs
  | Vector.null xs = []
  | otherwise = let (segment, remains) = Vector.splitAt n xs
                in (segment:chunkVector n remains)

instance Show Caves where
  show (Caves{..}) = squares &
                     fmap toChar &
                     chunkVector width &
                     fmap show &
                     unlines
    where
directions :: [Point]
directions = [Point (-1) 0, Point 0 1, Point 1 0, Point 0 (-1)]

(<!>) :: Caves -> Point -> Square
(<!>) Caves{..} (Point r c) =
  squares ! (r * width + c)

neighbors :: Caves -> Point -> [Point]
neighbors caves point = isEmpty `filter` ((point <>) `fmap` directions)
  where
    isEmpty p = caves <!> p == Empty

isInRange :: Caves -> Race -> Point -> Bool
isInRange caves race point = undefined

unique :: Eq a => [a] -> Either String a
unique [] = Left "empty list"
unique (x:xs) = recur x xs
  where
    recur u [] = Right u
    recur u (x:xs) = if u == x
      then recur u xs else Left "multiple distinct characters"

parseCaves :: String -> Either String Caves
parseCaves s =
  let rows = lines s
      width = unique (length `fmap` rows)
      parseSquare c
        | c == '.' = Right Empty
        | c == '#' = Right Wall
        | c == 'E' = Right (Creature Elf)
        | c == 'G' = Right (Creature Goblin)
        | otherwise = Left $ [c] ++ " does not represent a valid square"
      squares :: Either String [Square]
      squares = parseSquare `mapM` concat rows
  in Caves <$> (Vector.fromList <$> squares) <*> width

readCaves :: FilePath -> IO (Either String Caves)
readCaves path = parseCaves <$> readFile path

data RoseTree a = RoseTree a [RoseTree a] deriving (Show)

paths :: (a -> Bool) -> RoseTree a -> [[a]]
paths pred tree = filter (not . null) $ paths' tree
  where
    paths' (RoseTree val branches)
      | pred val = [[val]]
      | otherwise = [(val:p) | branch <- branches, p <- paths pred branch]

spawnTree :: (a -> [a]) -> a -> RoseTree a
spawnTree gen start = RoseTree start (spawnTree gen `fmap` gen start)

pathTree :: Caves -> Point -> RoseTree Point
pathTree caves start = spawnTree gen start
  where gen point = awayFromStart point `filter` neighbors caves point
        awayFromStart p q = dist start q > dist start p
        dist (Point x y) (Point u v) = abs (x - u) + abs (y - v)

getRight :: Either a b -> b
getRight (Right b) = b

inRange :: Caves -> Square -> Point -> Bool
inRange caves square point =
  square `elem` (caves <!>) `fmap` (point <>) `fmap` directions

z = do
  caves <- getRight <$> readCaves "res/input-15-test.txt"
  let tree = pathTree caves (Point 1 3)
  -- TODO: It seems that separating the predicate on the square to reach has bad
  -- effect on performance.
  pure $ length <$> paths (inRange caves (Creature Elf)) tree

u = do
  caves <- getRight <$> readCaves "res/input-15-test.txt"
  print caves
  -- let tree = pathTree caves (Point 1 1)
  -- pure $ take 1 $ paths (\_ -> True) tree
  pure $ caves <!> Point 4 4

v = do
  caves <- getRight <$> readCaves "res/input-15-test.txt"
  putStrLn $ show caves




--  
