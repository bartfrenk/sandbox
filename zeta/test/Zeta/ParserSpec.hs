module Zeta.ParserSpec where

import           Test.Hspec
import qualified Data.Text as T

import           Zeta.Parser
import           Zeta.Syntax

spec :: Spec
spec =
  describe "simple fragments" $ do
    it "None literal" $
      "None" |=> Literal None
    it "integer literals" $
      "1" |=> Literal (I 1)
    it "literal True" $
      "True" |=> Literal (B True)
    it "literal False" $
      "False" |=> Literal (B False)
    it "string literals" $
      "\"hello\"" |=> Literal (S "hello")
    it "empty string literals" $
      "\"\"" |=> Literal (S "")
    it "let bindings" $
      let s = T.unlines
            [ "let x = 1"
            , "    y = 2"
            , "in 3"
            ]
      in s |=> Binding
         [("x", Literal (I 1)), ("y", Literal (I 2))]
         (Literal (I 3))
    it "external functions" $
      "external(urn:x:y)" |=> External ["x", "y"]
    it "function application of externals" $
      "external(urn:x)(z = 1, u = 2)" |=>
      App (External ["x"]) [("z", Literal (I 1)), ("u", Literal (I 2))]
    it "function application of vars" $
      "ext(z = 1, u = 2)" |=>
      App (Var "ext") [("z", Literal (I 1)), ("u", Literal (I 2))]
    it "variable" $
      "x" |=> Var "x"
    it "assignment" $
      "x = 1" |=> Assignment "x" (Literal (I 1))
    it "fetch" $
      "fetch(urn:x:y)" |=> Fetch ["x", "y"]
    it "plus operators" $
      "1 + 2" |=> BinaryOp Plus (Literal (I 1)) (Literal (I 2))
    it "equality comparison" $
      "3 == 4" |=> BinaryOp Equals (Literal (I 3)) (Literal (I 4))
    it "left associativity" $
      "3 + 4 + 5" |=>
      BinaryOp Plus (BinaryOp Plus (Literal (I 3)) (Literal (I 4))) (Literal (I 5))
    it "times has higher precedence than plus" $
      "3 + 4 * 5" |=>
      BinaryOp Plus (Literal (I 3)) (BinaryOp Times (Literal (I 4)) (Literal (I 5)))
    it "parenthesis in arithmetic expression" $
      "3 + (4 + 5)" |=>
      BinaryOp Plus (Literal (I 3)) (BinaryOp Plus (Literal (I 4)) (Literal (I 5)))
    it "multiple expressions" $
      T.unlines ["fetch(urn:x)", "fetch(urn:y)"] |=> [Fetch ["x"], Fetch ["y"]]
  where

    s |=> expr = parse s `shouldBe` Right expr
