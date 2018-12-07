module Exercises where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Functor.Identity
import           Data.List             (foldl')
import           Data.List             (sort)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import qualified Data.Set              as Set
import           Data.Time.Format
import           Data.Time.Clock
import           Text.Parsec


type Parser s a = Parsec s () a

type CharStream s = Stream s Identity Char

parseChanges :: Monad m => String -> ExceptT ParseError m [Int]
parseChanges s = liftEither $ runParser (change `endBy` eol) () "" s
  where
    change = do
      sign <- oneOf "-+"
      value <- read <$> many1 digit
      pure $ if sign == '-' then -value else value
    eol = do
      string "\n"

readChanges :: FilePath -> ExceptT ParseError IO [Int]
readChanges path = (liftIO $ readFile path) >>= parseChanges

findFirstDuplicate :: [Int] -> Int
findFirstDuplicate ds = recurse Set.empty 0 $ cycle ds
  where
    recurse _ _ [] = error "Should not happen"
    recurse !seen !acc (x:xs) = do
      if acc `Set.member` seen
        then acc
        else recurse (Set.insert acc seen) (acc + x) xs

answer1a :: IO (Either ParseError Int)
answer1a = runExceptT (foldl' (+) 0 <$> readChanges "res/frequency-changes.txt")

answer1b :: IO  (Either ParseError Int)
answer1b = runExceptT (findFirstDuplicate <$> readChanges "res/frequency-changes.txt")

readLines :: MonadIO m => FilePath -> m [String]
readLines path = lines <$> (liftIO $ readFile path)

score :: String -> (Int, Int)
score cs =
  let m = count Map.empty cs
      ns = snd <$> (Map.toList m)
  in (if 2 `elem` ns then 1 else 0, if 3 `elem` ns then 1 else 0)
  where
    count m [] = m
    count m (c:css) = count (Map.insertWith (+) c 1 m) css


answer2a :: IO Int
answer2a = do
  lines <- readLines "res/letters.txt"
  let scores = score <$> lines
  let (twos, threes) = foldl' sumPairs (0, 0) scores
  pure $ twos * threes
  where
    sumPairs (x, y) (u, v) = (x + u, y + v)


answer2b :: IO String
answer2b = do
  lines <- readLines "res/letters.txt"
  let (x, y) = head $ connected lines
  pure $ remove x y
  where
    remove (c:cs) (d:ds) = if c == d then (c:remove cs ds) else remove cs ds
    remove _ _ = []

connected :: [String] -> [(String, String)]
connected zs = do
  x <- zs
  y <- zs
  if dist x y == 1 then [(x, y)] else []

dist :: String -> String -> Int
dist s t = dist' 0 s t
  where
    dist' acc [] _ = acc
    dist' acc _ [] = acc
    dist' !acc (c:cs) (d:ds) =
      if c == d
      then dist' acc cs ds
      else dist' (acc + 1) cs ds

data Claim = Claim !Int !Int !Int !Int !Int deriving (Eq, Show)

parseClaim :: Monad m => String -> ExceptT ParseError m Claim
parseClaim s = liftEither $ runParser claim () "" s
  where
    claim = do
      char '#'
      id <- number
      space
      char '@'
      space
      left <- number
      char ','
      top <- number
      char ':'
      space
      width <- number
      char 'x'
      height <- number
      pure $ Claim id left top width height
    number = read <$> many1 digit

readClaims :: FilePath -> ExceptT ParseError IO [Claim]
readClaims path = do
  lines <- liftIO $ readLines path
  mapM parseClaim lines

claims :: IO (Either ParseError [Claim])
claims = runExceptT (readClaims "res/fabric-claims.txt")

data Covered = Once | Overlap deriving (Eq, Show)

computeOverlap :: [Claim] -> Int
computeOverlap claims = countOverlaps (recur Map.empty claims)
  where
    recur !fabric [] = fabric
    recur !fabric (c:cs) = recur (updateWithClaim fabric c) cs
    updateWithClaim fabric (Claim _ left top width height) =
      let coords = [(x, y) | x <- [left .. left + width - 1], y <- [top .. top + height - 1]]
      in foldl' (\m p -> Map.insertWith (\_ -> const Overlap) p Once m) fabric coords
    countOverlaps fabric = length (filter (\(_, cov) -> cov == Overlap) $ Map.toList fabric)

answer3a :: IO (Either ParseError Int)
answer3a = runExceptT $ computeOverlap <$> readClaims "res/fabric-claims.txt"

overlaps :: Claim -> Claim -> Bool
overlaps (Claim _ left1 top1 width1 height1) (Claim _ left2 top2 width2 height2) =
  overlappingIntervals (left1, left1 + width1) (left2, left2 + width2) &&
  overlappingIntervals (top1, top1 + height1) (top2, top2 + height2)
  where overlappingIntervals (x, y) (u, v) = not (v <= x) && not (y <= u)

nonOverlapping :: [Claim] -> Maybe Claim
nonOverlapping all = recur all
  where
    recur [] = Nothing
    recur (c:cs) =
      let cs' = filter (overlaps c) all
      in if cs' == [c] then Just c else recur $ filter (not . overlaps c) cs

answer3b :: IO (Either ParseError (Maybe Claim))
answer3b = runExceptT $ nonOverlapping <$> readClaims "res/fabric-claims.txt"

data Event
  = ShiftStarts Int
  | FallsAsleep
  | WakesUp
  deriving (Eq, Show)

data Entry = Entry UTCTime Event deriving (Eq, Show)

instance Ord Entry where
  Entry t _ `compare` Entry s _ = t `compare` s



parseEntry :: Monad m => String -> ExceptT ParseError m Entry
parseEntry = liftEither . runParser (Entry <$> time <*> event) () ""
  where
    time = do
      char '[' *> time' <* string "] "
    time' =
      let format = "%Y-%m-%d %H:%M"
      in (many1 $ noneOf "]") >>= parseTimeM False defaultTimeLocale format
    event =  choice [shiftStarts, fallsAsleep, wakesUp]
    shiftStarts = do
      string "Guard #"
      id <- read <$> many1 digit
      string " begins shift"
      pure $ ShiftStarts id
    fallsAsleep = string "falls asleep" >> pure FallsAsleep
    wakesUp = string "wakes up" >> pure WakesUp

readEntries :: FilePath -> ExceptT ParseError IO [Entry]
readEntries path = do
  lines <- readLines path
  entries <- mapM parseEntry lines
  pure $ sort entries

selectGuard :: [Entry] -> (Int, NominalDiffTime)
selectGuard ((Entry _ (ShiftStarts id)):es) =
  let minutes = recur Map.empty id Nothing es
  in foldl1 (\(g, t) (h, s) -> if s > t then (h, s) else (g, t)) (Map.toList minutes)
  where
    recur acc _ _ [] = acc
    recur acc id time ((Entry _ (ShiftStarts id')):es)
      = recur acc id' Nothing es
    recur acc id time ((Entry t FallsAsleep):es)
      = recur acc id (Just t) es
    recur acc id (Just t') ((Entry t WakesUp):es)
      = recur (Map.insertWith (+) id (diffUTCTime t t') acc) id Nothing es

sleep :: [Entry] -> Map Int [(UTCTime, UTCTime)]
sleep ((Entry _ (ShiftStarts id)):es) = recur Map.empty id Nothing es
  where
    recur acc _ _ [] = acc
    recur acc id time ((Entry _ (ShiftStarts id')):es)
      = recur acc id' Nothing es
    recur acc id time ((Entry t FallsAsleep):es)
      = recur acc id (Just t) es
    recur acc id (Just t') ((Entry t WakesUp):es)
      = recur (Map.insertWith (++) id [(t', t)] acc) id Nothing es

-- countMinutes :: Int -> [Entry] -> Int
-- countMinutes id entries =
--   let sl = sleep entries
--       intervals = sl Map.! 727
--   in do
--     (start, end) <- intervals
--     undefined


bla :: IO (Either ParseError (Maybe [(UTCTime, UTCTime)]))
bla = runExceptT $ ((Map.lookup 727) . sleep <$> readEntries "res/guard-entries.txt")
