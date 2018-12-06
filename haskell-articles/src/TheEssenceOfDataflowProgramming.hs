module TheEssenceOfDataflowProgramming where


-- class Comonad d where
--   counit :: d a -> a
--   cobind :: (d a -> b) -> d a -> d b

-- cmap :: Comonad d => (a -> b) -> d a -> d b
-- cmap f = cobind (f . counit)


-- data Id a = Id a

-- data Prod e a = Prod (a, e)

-- instance Comonad Id where
--   counit (Id a) = a
--   cobind f = Id . f

-- instance Comonad (Prod e) where
--   counit (Prod (a, e)) = a
--   cobind f d@(Prod (a, e)) = Prod (f d, e)


-- data Stream a = a :< Stream a

-- instance Comonad Stream where
--   counit (a :< _) = a
--   cobind f d@(a :< as) = (f d) :< cobind f as

-- nextS :: Stream a -> Stream a
-- nextS (_ :< s) = s

-- data Prod' e a = a :& e

-- data FunArg s a = (s -> a) :# s

-- instance Comonad (FunArg s) where
--   counit (f :# s) = f s
--   cobind k (f :# s) = (\s' -> k (f :# s')) :# s

-- -- Next Paragraph 5.2 Comonads for general and causal stream functions

-- data List a = Nil | List a :> a

-- data LV a = List a := a

-- data LVS a = LV a :| Stream a


-- instance Comonad LVS where
--   counit (past := present :| future) = present
--   cobind k d

class Functor w => Comonad w where
  (=>>) :: w a -> (w a -> b) -> w b
  counit :: w a -> a
  cojoin :: w a -> w (w a)
  x =>> f = fmap f (cojoin x)

instance Functor U where
  f `fmap` U as b cs = U (f `fmap` as) (f b) (f `fmap` cs)

instance Comonad U where
  counit (U _ x _) = x
  cojoin u = U (tail $ iterate left u) u (tail $ iterate right u)

-- From http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html
-- One-dimensional cellular automata are comonads

data U x = U [x] x [x]

right :: U x -> U x
right (U as b (c:cs)) = U (b:as) c cs

left :: U x -> U x
left (U (a:as) b cs) = U as a (b:cs)

rule :: U Bool -> Bool
rule (U (a:_) b (c:_)) = not (a && b && not c || (a == b))

-- The rest is just for printing

shift i u = (iterate (if i < 0 then left else right) u) !! abs i

toList :: Int -> Int -> U a -> [a]
toList i j u = take (j-i) $ half $ shift i u
  where
    half (U _ b c) = [b] ++ c

test scale =
  let u = U (repeat False) True (repeat False)
  in putStr $
     unlines $
     take scale $
     map (map (\x -> if x then '#' else ' ') . toList (-scale) scale) $
     iterate (=>> rule) u


-- Function interpolation

data Interpolate y = Interpolate (Float -> y) Float



instance Functor Interpolate where
  f `fmap` (Interpolate h x) = Interpolate (f . h) x

instance Comonad Interpolate where
  counit (Interpolate h x) = h x
  cojoin (Interpolate h x) = Interpolate k x
    where k x' = Interpolate h x'


-- https://bartoszmilewski.com/2017/01/02/comonads/
-- https://pdfs.semanticscholar.org/ad77/ebcb739925559b48adc441d86ea45e7b9900.pdf
-- http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html
