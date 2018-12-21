module Snark where

-- Ingredients:
-- 1. Translate arithmetic expressions to a QAP
-- 2. Verifiable blind evaluation of polynomials

{--
Let E be an homomorphic hiding,
1. For most x, given E x it
2. Injective
3. Homomorphic
--}
type HomomorphicHiding a b = a -> b

data Expr
  = Mult Expr Expr
  | Sum Expr Expr
  | IntLiteral Int

data Polynomial a = Polynomial [a]

data Var = Var Int

-- | Galois field element
data GF = GF
  { size :: Int
  , elt :: Int
  }

-- | Here c is the type of some finite field (of prime order)
type QAP c = Polynomial (Either Var c)

computeQAP :: Expr -> QAP GF
computeQAP = undefined

computeTargetPolynomial :: Expr -> Polynomial GF
computeTargetPolynomial = undefined


{-- Alice aims to prove that she as a valid assignment, e.g., a collection of
parameter assignments to the input of a function that map to some specified
output.

The expression has an associated set of component polynomials (the wire
polynomials). These are combined with the parameter assignments yields new
polynomial: L, R, O

1. If L, R and O are linear combinations of the component polynomials (here we
seem to need the knowledge of coefficient assumption) then proving that L * R -
O is divisible by the target polynomial is equivalent to the statement that the
coefficients of linearity are valid assignments.

Alice needs to prove that she knows polynomials L, R, O and H s.t.

1. L, R, O are the same linear combination of their respective wire polynomials
2. L * R - O = H * T


--}
