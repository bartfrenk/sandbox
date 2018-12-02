module Exercises where

import Control.Monad.Except
import Control.Monad
import Control.Monad.State
import Data.List (foldl')
import Data.Functor.Identity
import Text.Parsec
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set

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

readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path

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
