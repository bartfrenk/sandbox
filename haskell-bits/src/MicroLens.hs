module MicroLens where

import Lens.Micro.Platform
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Semigroup ((<>))

-- Essentially two operations:
--
-- set (.~): set focus to a value
-- over (%~): apply a function to the focus

x :: (Int, Int)
x = (1, 2 :: Int) & _2 .~ 1

y :: (Int, Int)
y = (1, 2) & _1 %~ negate

z :: (Int, Int)
z = (1, 2) & _2 +~ 1

m :: Map Int Int
m = Map.empty & at 3 ?~ 1
              & at 2 ?~ 2
              & at 3 ?~ 4


(+~) :: Num a => ASetter s t a a -> a -> s -> t
ln +~ n = ln %~ (+ n)

z1 :: Char
z1 = ('x', 'y') ^. _1


-- z2 :: Int
-- z2 = [(1, 2), (2, 3), (3, 4)] ^. ix 1 . _2

hello :: String
hello = ("hello", " ",  "world!") ^. each


world :: [Int]
world = (1 :: Int, 2, 3) ^.. (_2 <> _3)


bla :: [Int]
bla = ([1, 2], 3, [Nothing, Just 4]) ^.. (_1.each <> _2 <> _3.each._Just)

baz = [0..5] ^? ix 2


quux :: [Int]
quux = [4, 1, 2, 3] & _tail %~ reverse



-- blabla :: [Int]
-- blabla = ([1, 2], 3, [Nothing, Just 4]) & (_3.each._Just) %~ reverse



