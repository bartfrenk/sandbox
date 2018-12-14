{-# LANGUAGE StrictData #-}
module Day2 where

import Data.Maybe
import Data.List
import Data.Function
import Control.Monad.Except
import Data.Functor.Identity
import Text.Parsec

type Parser s a = Parsec s () a

type CharStream s = Stream s Identity Char

data Pots a = Pots ![a] !a ![a]

right (Pots a b (c:cs)) = Pots (b:a) c cs
left  (Pots (a:as) b c) = Pots as a (b:c)

instance Functor Pots where
  fmap f (Pots left c right) = Pots (fmap f left) (f c) (fmap f right)

class Functor w => Comonad w where
  (=>>) :: w a -> (w a -> b) -> w b
  coreturn :: w a -> a
  cojoin :: w a -> w (w a)
  x =>> f = fmap f (cojoin x)

instance Comonad Pots where
  cojoin a = Pots (tail $ iterate left a) a (tail $ iterate right a)
  coreturn (Pots _ c _) = c

makeList :: Int -> Pots a -> [(Int, a)]
makeList n (Pots left c right) =
  let leftReverse = reverse (take n left)
  in (zip [-1..] leftReverse) ++ [(0, c)] ++ take n (zip [1..] right)

withIndex :: Pots a -> Pots (Int, a)
withIndex (Pots left c right) = Pots (zip [-1,-2..] left) (0, c) (zip [1,2..] right)

showPots :: Int -> Pots Bool -> String
showPots n (Pots left c right) =
  let leftReverse = reverse (take n left)
  in map sym (leftReverse ++ [c] ++ take n right)

  where
    sym True = '#'
    sym False = '.'

input :: String
input =
  ".##.##...#.###..#.#..##..###\
        \..##...####.#...#.##....##.#\
        \.#...#...###.........##...##\
        \#.....##.##.##"

patterns :: [String]
patterns =
  [ "##... => ."
  , "#...# => ."
  , ".###. => #"
  , ".##.# => #"
  , "#.... => ."
  , "..##. => #"
  , "##..# => #"
  , ".#... => #"
  , ".#.## => #"
  , "#.### => #"
  , ".#..# => ."
  , "##.#. => #"
  , "..#.. => ."
  , ".##.. => #"
  , "###.# => ."
  , ".#### => ."
  , "##### => ."
  , "#.#.. => #"
  , "...## => #"
  , "...#. => ."
  , "###.. => ."
  , "..... => ."
  , "#.#.# => ."
  , "##.## => #"
  , "#.##. => #"
  , "####. => #"
  , "#..#. => #"
  , ".#.#. => ."
  , "#..## => #"
  , "....# => ."
  , "..#.# => #"
  , "..### => ."
  ]

readPots :: Monad m => String -> ExceptT ParseError m (Pots Bool)
readPots = liftEither . runParser pots () ""
  where
    pots = do
      c <- pot
      right <- many pot
      pure $ Pots (repeat False) c (right ++ repeat False)

pot :: Parser String Bool
pot = do
  c <- oneOf ".#"
  if | c == '#' -> pure True
     | c == '.' -> pure False
     | otherwise -> fail "Not a pot"

type Pattern = ((Bool, Bool, Bool, Bool, Bool), Bool)
type Rule a = Pots a -> a

readPattern :: Monad m => String -> ExceptT ParseError m Pattern
readPattern = liftEither . runParser rulePattern () ""
  where
    rulePattern = (,) <$> (guard <* string " => ") <*> pot
    guard = (,,,,) <$> pot <*> pot <*> pot <*> pot <*> pot

makeRule :: [Pattern] -> Rule Bool
makeRule pats (Pots (b:a:_) c (d:e:_)) =
  fromJust $ lookup (a, b, c, d, e) pats

pat = ((False, False, False, False, False), True)
ps = Pots (repeat False) False (repeat False)

test1 :: IO (Either ParseError ())
test1 = runExceptT $ do
  pots <- readPots input
  liftIO $ putStrLn $ showPots 100 pots

test2 :: IO (Either ParseError Bool)
test2 = runExceptT $ do
  pots <- readPots input
  pats <- mapM readPattern patterns
  pure $ makeRule pats pots

test3 :: IO (Either ParseError (Rule Bool))
test3 = runExceptT $ do
  pats <- mapM readPattern patterns
  pure $ makeRule pats

answer12 :: Int -> IO (Either ParseError Int)
answer12 n= runExceptT $ do
  pots <- readPots input
  pats <- mapM readPattern patterns

  let gen = head $ drop n $ iterate (=>> makeRule pats) pots

  let (Pots left _ right) = withIndex gen &
        fmap (\case (i, True) -> i; (_, False) -> 0)

  pure $ foldl' (+) 0 (take (10 * n) left) +
    foldl' (+) 0 (take (10 * n) right)
