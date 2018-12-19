module Day13 where

import           Data.Function    ((&))
import           Data.List
import           Data.Maybe
import           Data.Vector      (Vector, (!), (//))
import qualified Data.Vector      as Vector
import           Debug.Trace
import           Prelude          hiding (readFile)
import           System.IO.Strict (readFile)


extractUnique :: Eq a => [a] -> Maybe a
extractUnique [] = Nothing
extractUnique (x:xs) = recur x xs
  where
    recur u [] = Just u
    recur u (x:xs) = if u == x then recur u xs else Nothing

data Tracks = Tracks (Vector Char) Int

instance Show Tracks where
  show (Tracks chars width) = recur [] chars
    where
      recur acc remains
        | Vector.length remains > 0 =
            let (line, remains') = Vector.splitAt width remains
            in recur (acc ++ "\n" ++ Vector.toList line) remains'
        | otherwise = acc

parseTracks :: String -> Maybe Tracks
parseTracks s =
  let xs = lines s
  in Tracks (Vector.fromList $ concat xs)
     <$> extractUnique (length `fmap` xs)

getSymbol :: Tracks -> (Int, Int) -> Char
getSymbol (Tracks chars w) (x, y) = chars ! (y * w + x)

data NextTurn = L | S | R deriving (Show, Eq)

data Direction = North | South | East | West deriving (Show, Eq)

data Cart = Cart (Int, Int) Direction NextTurn deriving (Show, Eq)

instance Ord Cart where
  (Cart (x, y) _ _) `compare` (Cart (u, v) _ _) =
    let cmp = y `compare` v
    in if cmp == EQ then x `compare` u else cmp

extractCarts :: Tracks -> Vector Cart
extractCarts (Tracks chars width) =
  Vector.indexed chars &
  Vector.filter ((`elem` "<>^v") . snd) &
  fmap (\(idx, symbol) -> Cart (toCoords idx) (direction symbol) L)
  where
    toCoords idx = let (y, x) = quotRem idx width in (x, y)
    direction s
      | s == '^' = North
      | s == '>' = East
      | s == '<' = West
      | s == 'v' = South
      | otherwise = error $ "Not a valid cart symbol: '" ++ [s, '\'']

moveCart :: Tracks -> Cart -> Either (Cart, Char) Cart
moveCart tracks cart@(Cart coords dir nextTurn) =
  if | covered `elem` "-|v^<>\\/+" ->
         let coords' = moveDirection dir coords
             (nextTurn', dir') = updateDirection coords' dir nextTurn
         in Right $ Cart coords' dir' nextTurn'
     | otherwise -> Left (cart, covered)

  where
    covered = getSymbol tracks coords

    moveDirection North (x, y) = (x, y - 1)
    moveDirection South (x, y) = (x, y + 1)
    moveDirection East (x, y) = (x + 1, y)
    moveDirection West (x, y) = (x - 1, y)

    cycleTurn L = S
    cycleTurn S = R
    cycleTurn R = L

    makeTurn North L = West
    makeTurn North R = East
    makeTurn South L = East
    makeTurn South R = West
    makeTurn East L = North
    makeTurn East R = South
    makeTurn West L = South
    makeTurn West R = North
    makeTurn dir _ = dir

    updateDirection coords' dir turn =
      let covered' = getSymbol tracks coords'
      in if | covered' `elem` "|-<>^v" -> (turn, dir)
            | covered' == '\\' -> (turn,
                case dir of
                  North -> West; East -> South; South -> East; West -> North)
            | covered' == '/' -> (turn,
                case dir of
                  North -> East; East -> North; South -> West; West -> South)
            | covered' == '+' ->
                (cycleTurn turn, makeTurn dir turn)
            | otherwise -> error $ "Not a valid track symbol: '" ++ [covered', '\'']

tick :: Tracks -> Vector Cart -> Either (Cart, Vector Cart) (Vector Cart)
tick tracks carts = recur 0 carts
  where
    n = length carts
    recur idx carts' =
      if idx < n
      then let cart@(Cart coords _ _)  = case moveCart tracks (carts' ! idx) of
                                           Right cart -> cart
                                           Left (cart, c) ->
                                             trace (show $ (c, carts' ! idx)) cart
               collisions = flip Vector.ifilter carts' $
                 \i (Cart coords' _ _) -> (coords == coords') && (idx /= i)
           -- TODO: update is linear in the length of the vector
           in if Vector.null collisions
              then recur (idx + 1) (carts' // [(idx, cart)])
              else Left (cart, collisions)
      else Right carts'


tick' :: Tracks -> [Cart] -> [Cart]
tick' tracks carts = recur [] carts
  where
    recur moved [] = moved
    recur moved (cart:remaining) =
      let (Right cart') = moveCart tracks cart
          (moved', moved'') = partition (collide cart') moved
          (remain', remain'') = partition (collide cart') remaining
      in if null moved' && null remain'
         then recur (cart':moved'') remain''
         else recur moved'' remain''

    collide (Cart x _ _) (Cart y _ _) = x == y

-- TODO: converting to and from a list is likely to be costly
sortVector :: Ord a => Vector a -> Vector a
sortVector v = Vector.fromList $ sort (Vector.toList v)

firstCollision :: Tracks -> Vector Cart -> (Cart, Vector Cart)
firstCollision tracks carts = recur carts
  where
    recur carts' =
      case tick tracks (sortVector carts') of
        Right carts'' -> recur carts''
        Left (cart, collisions) -> (cart, collisions)

lastCart :: Tracks -> [Cart] -> Maybe Cart
lastCart tracks carts = recur carts
  where
    recur carts' =
      case tick' tracks (sort carts') of
        [] -> Nothing
        [cart] -> Just cart
        carts'' -> recur carts''

answer13a = do
  tracks <- fromJust . parseTracks <$> readFile "res/input-13-test-3.txt"
  let carts = extractCarts tracks
  pure $ firstCollision tracks carts

answer13b = do
  tracks <- fromJust . parseTracks <$> readFile "res/input-13.txt"
  let carts = Vector.toList $ extractCarts tracks
  pure $ lastCart tracks carts
