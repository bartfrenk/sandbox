import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (foldl')
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map

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


main :: IO ()
main = do
  answer2a >>= putStrLn . ("Part A: " ++) . show
  answer2b >>= putStrLn . ("Part B: " ++) . show
