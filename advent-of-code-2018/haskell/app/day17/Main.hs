{-# LANGUAGE ScopedTypeVariables #-}
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
import           Text.Parsec                 hiding (State)

import           Debug.Trace

main :: IO ()
main = putStrLn "day 17"

-- |Representation of points in the ground to be irrigated.
type Point = (Int, Int)

-- |The irrigated ground.
data Ground v = Ground
  { squares :: v Char
  , bounds  :: Block
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



-- |Simple wrapper to indicate the state of the irrigation process.
data State v = State
  { ground :: Ground v
  , active :: [Point]
  }

-- |Takes the active water square at the head of the list of active squares, and
-- fills the appropriate surrounding squares with water, depending only on the
-- neighborhood of the active square. Prefixes points to the active list.
stepM :: (PrimMonad m, GM.MVector v Char)
      => Ground (v (PrimState m)) -> [Point] -> m [Point]
stepM g [] = pure []
stepM g active@(p:rest) = do
  switchM
    [ (peek here '+') |&& (peek down '.'), (put down '|') >> pure ((down p):active)

      ((g <!?> p `isM` '+') `andM` (g <!?> (down p) `isM` '.'),
       put g (down p) '|' >> pure (down p:active))

    , ((g <!?> p `isM` '|') `andM` (g <!?> (down p) `isM` '.'),
       put g (down p) '|' >> pure (down p:active))

    , ((g <!?> p `isM` '|') `andM` (g <!?> (down p) `isM` '#'),
       pure rest)

    , (pure True, pure [])

    , ((peek here '|') |&& (peek down '.'), pure [])
    ]
  where
    here = id
    peek loc c = (g <!?> (loc p)) `isM` c
    down (x, y) = (x, y + 1)
    isM a b = (== (Just b)) <$> a
    andM = liftM2 (&&)
    (|&&) = andM
    whenM cond act = do
      b <- cond
      if b then act else pure ()

switchM :: Monad m => [(m Bool, m a)] -> m a
switchM [] = error "No condition matches"
switchM ((x, a):xs) = do
  b <- x
  if b then a else switchM xs

put :: (PrimMonad m, GM.MVector v Char)
    => Ground (v (PrimState m)) -> (Int, Int) -> Char -> m ()
put Ground{..} p c =
  let idx = toIndex bounds p
  in GM.write squares idx c

(<!?>) :: (PrimMonad m, GM.MVector v Char)
       => Ground (v (PrimState m)) -> (Int, Int) -> m (Maybe Char)
Ground{..} <!?> p =
  let idx = toIndex bounds p
  in if idx > GM.length squares
     then pure $ Nothing
     else Just <$> GM.unsafeRead squares idx

-- process ground p
--   | ground <!> p == '+' && ground <!> below p = Just '.' put ground (below p) '|'

evolveN :: forall v. G.Vector v Char => Int -> Ground v -> [Point] -> Ground v
evolveN n ground active = Ground squares' (bounds ground)
  where
    update msquares =
      void (loopM n (stepM $ Ground msquares (bounds ground)) active)
    squares' =  G.modify update (squares ground)

-- |Variant of 'iterateM' that stops after 'n' iterations.
loopM :: Monad m => Int -> (a -> m a) -> a -> m a
loopM n act v = recur n v
  where
    recur n' v'
      | n' == 0 = pure v'
      | n' > 0 = act v' >>= recur (n' - 1)
      | otherwise = error "Iteration count needs to be positive"

-- === PARSING ===

data Block = Block
  { x :: (Int, Int)
  , y :: (Int, Int)
  } deriving (Show)

parseGround :: Monad m => String -> ExceptT ParseError m (Ground U.Vector)
parseGround s = liftEither $ do
  blocks <- runParser (endBy block newline) () "" s
  let bounds = computeBounds blocks
  let v = U.modify (putBlocks bounds blocks) (U.replicate (size bounds) '.')
  pure $ Ground v bounds
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
computeBounds :: [Block] -> Block
computeBounds [] = error "No blocks"
computeBounds (block:rest) = expand $ foldl' coverBlock block rest
  where
    coverBlock (Block x y) (Block u v) =
      Block (coverRange x u) (coverRange y v)
    coverRange (a, b) (c, d) = (min a c, max b d)
    expand (Block (lo, hi) (_, r)) = Block (lo - 1, hi + 1) (0, r)


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

fromRight :: Either a b -> b
fromRight (Right b) = b

t :: IO ()
t = do
  ground <- fromRight <$> (runExceptT $ parseGround sample)
  print $ evolveN 7 ground [(500, 0)]

-- Approach:

-- Use a vector, keep active points, and expand the active points based on local
-- rules. Since this is mainly doing updates, and many of them, I think we
-- a mutable vector is appropriate.
