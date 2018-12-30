-- Essentially a recursive solution, but awkwardly encoded using a list of
-- active points, instead of letting the runtime keep track of the work still to
-- be done.

-- Works, but not too happy with the final form. Maybe rewrite in actual
-- recursive form for comparison.

{-# LANGUAGE ScopedTypeVariables #-}
import           Prelude                     hiding (readFile)

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Data.Function               ((&))
import           Data.List
import           Data.Vector.Generic         ((!))
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           System.IO.Strict            (readFile)
import           Text.Parsec                 hiding (State)

import           Debug.Trace

main :: IO ()
main = readFile "res/input-17.txt" & fmap parseGround >>= \case
  Left err -> putStrLn $ show err
  Right init -> do
    let irrigated = evolve init [(500, 0)]
    -- The -7 correction is to avoid counting the water squares that lie above
    -- the tip of the highest basin (i.e., have y-coordinate smaller than any
    -- y-coordinate appearing in the input data).
    putStrLn $ "Part A: " ++ show ((countWater "~|" irrigated) - 7)
    putStrLn $ "Part B: " ++ show (countWater "~" irrigated)

-- |Representation of points in the ground to be irrigated.
type Point = (Int, Int)

-- |The irrigated ground.
data Ground v = Ground
  { squares :: v Char
  , bounds  :: Block
  , minY :: Int
  }

instance Show (Ground U.Vector) where
  show Ground{..} =
    let (lo, hi) = x bounds
        width = hi - lo + 1
    in squares & chunkVector width & fmap show & unlines
    where
      chunkVector n xs
        | U.null xs = []
        | otherwise = let (segment, remains) = U.splitAt n xs
                      in (segment:chunkVector n remains)

-- |Takes the active water square at the head of the list of active squares, and
-- fills the appropriate surrounding squares with water, depending only on the
-- neighborhood of the active square. Prefixes points to the active list.
stepM :: (PrimMonad m, GM.MVector v Char)
      => Ground (v (PrimState m)) -> [Point] -> m [Point]
stepM g [] = pure []
stepM g active@(center:rest) = do
  let down = move (0, 1)
      up = move (0, -1)
      left = move (-1, 0)
      right = move (1, 0)
  downC <- peek down
  upC <- peek up
  leftC <- peek left
  rightC <- peek right
  centerC <- peek center
  if | centerC === '+' && downC === '.' ->
       put down '|' >> pure (down:active)

     | centerC === '+' && downC === '|' ->
       pure rest

     | centerC === '|' && downC === '.' ->
       put down '|' >> pure (down:active)

     | centerC === '|' && (downC == Nothing || downC === '|') ->
       pure rest

     | centerC === '|' && (leftC === '~' || leftC === '|' ||
                           rightC === '|' || rightC === '~') -> do
         fromLeft <- if
           | leftC === '~' || leftC === '.' -> put left '|' >> pure [left]
           | otherwise -> pure []
         fromRight <- if
           | rightC === '~' || rightC === '.' -> put right '|' >> pure [right]
           | otherwise -> pure []
         pure $ fromLeft ++ fromRight ++ rest

     | centerC === '|' && (downC === '#' || downC === '~') -> do
         fromLeft <- if
           | leftC === '.' || leftC === '|' || leftC === '~' -> put left '~' >> pure [left]
           | otherwise -> pure []
         fromRight <- if
           | rightC === '.' || rightC === '|' || rightC === '~' -> put right '~' >> pure [right]
           | otherwise -> pure []
         put center '~'
         pure $ fromLeft ++ fromRight ++ rest

     | centerC === '~' && (downC === '#' || downC === '~') -> do
         fromLeft <- if
           | leftC === '.' || leftC === '|' -> put left '~' >> pure [left]
           | otherwise -> pure []
         fromRight <- if
           | rightC === '.' || rightC === '|' -> put right '~' >> pure [right]
           | otherwise -> pure []
         pure $ fromLeft ++ fromRight ++ rest

     | centerC === '~' && downC === '.' -> do
         put down '|'
         pure $ (down:active)

     | centerC === '~' && downC === '|' -> do
         fromLeft <- if
           | leftC === '~' -> put left '|' >> pure [left]
           | otherwise -> pure []
         fromRight <- if
           | rightC === '~' -> put right '|' >> pure [right]
           | otherwise -> pure []
         put center '|'
         pure $ [center] ++ fromLeft ++ fromRight ++ rest

     | otherwise -> pure rest

  where
    move (u, v) = (u + fst center, v + snd center)
    peek q = g <!?> q
    put q c = writeSquare g q c
    (===) (Just a) b = a == b
    (===) Nothing _ = False

writeSquare :: (PrimMonad m, GM.MVector v Char)
    => Ground (v (PrimState m)) -> (Int, Int) -> Char -> m ()
writeSquare Ground{..} p c =
  let idx = toIndex bounds p
  in GM.write squares idx c

(<!?>) :: (PrimMonad m, GM.MVector v Char)
       => Ground (v (PrimState m)) -> (Int, Int) -> m (Maybe Char)
Ground{..} <!?> p =
  let idx = toIndex bounds p
  in if idx > GM.length squares
     then pure $ Nothing
     else Just <$> GM.unsafeRead squares idx

-- |Run stepM until no active points remain.
evolve :: forall v. G.Vector v Char => Ground v -> [Point] -> Ground v
evolve ground active = Ground squares' (bounds ground) (minY ground)
  where
    update ps msquares = do
      ps' <- stepM (Ground msquares (bounds ground) (minY ground)) ps
      if null ps' then pure ()
      else update ps' msquares
    squares' =  G.modify (update active) (squares ground)

-- |Count the number of water squares (e.g., the squares in 'water').
countWater :: G.Vector v Char => String -> Ground v -> Int
countWater water (Ground {squares}) =
  G.length (G.filter (`elem` water) squares)


-- === PARSING ===

data Block = Block
  { x :: (Int, Int)
  , y :: (Int, Int)
  } deriving (Show)

parseGround :: String -> Either ParseError (Ground U.Vector)
parseGround s = do
  blocks <- runParser (endBy block newline) () "" s
  let (bounds, minY) = computeBounds blocks
  let v = U.modify (putBlocks bounds blocks) (U.replicate (size bounds) '.')
  pure $ Ground v bounds minY
  where
    horizontal = do
      _ <- string "y="
      n <- number
      _ <- string ", x="
      r <- range
      pure $ Block r (n, n)
    vertical = do
      _ <- string "x="
      n <- number
      _ <- string ", y="
      r <- range
      pure $ Block (n, n) r
    number = read <$> many1 digit
    range = (,) <$> (number <* string "..") <*> number
    block = horizontal <|> vertical

    size (Block (a, b) (c, d)) = (b - a + 1) * (d - c + 1)

    putBlocks bounds blocks v = do
      UM.write v (toIndex bounds (500, 0)) '+'
      recur blocks
      where
        recur [] = pure ()
        recur ((Block (a, b) (c, d)):rest) =
          let positions = [(x, y) | x <- [a..b], y <- [c..d]]
          in do
            forM_ positions $ \p -> UM.write v (toIndex bounds p) '#'
            recur rest

-- |Map the Cartesian coordinates (with left-top being (0, 0)) to an index into
-- the Char vector that has the squares.
toIndex :: Block -> (Int, Int) -> Int
toIndex (Block (a, b) (c, _)) (x, y) =
  let width = b - a + 1
  in (y - c) * width + (x - a)

-- |Compute the bounds of the map that we need to take into consideration to
-- solve the puzzle. Leaves two lines at the left and right edges, none at top and
-- bottom.
computeBounds :: [Block] -> (Block, Int)
computeBounds [] = error "No blocks"
computeBounds (block:rest) = expand $ foldl' coverBlock block rest
  where
    coverBlock (Block x y) (Block u v) =
      Block (coverRange x u) (coverRange y v)
    coverRange (a, b) (c, d) = (min a c, max b d)
    expand (Block (lo, hi) (minY, r)) = (Block (lo - 1, hi + 1) (0, r), minY)

sample :: String
sample = unlines
  [ "x=495, y=2..7"
  , "y=7, x=495..501"
  , "x=501, y=3..7"
  , "x=498, y=2..4"
  , "x=506, y=1..2"
  , "x=498, y=10..13"
  , "x=504, y=10..13"
  , "y=13, x=498..504"
  ]
