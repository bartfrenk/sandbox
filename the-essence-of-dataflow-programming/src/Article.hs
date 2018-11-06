module Article where


class Comonad d where
  counit :: d a -> a
  cobind :: (d a -> b) -> d a -> d b

cmap :: Comonad d => (a -> b) -> d a -> d b
cmap f = cobind (f . counit)


data Id a = Id a

data Prod e a = Prod (a, e)

instance Comonad Id where
  counit (Id a) = a
  cobind f = Id . f

instance Comonad (Prod e) where
  counit (Prod (a, e)) = a
  cobind f d@(Prod (a, e)) = Prod (f d, e)


data Stream a = Cons a (Stream a)

instance Comonad Stream where
  counit (Cons a _) = a
  cobind f d@(Cons a as) = Cons (f d) (cobind f as)
