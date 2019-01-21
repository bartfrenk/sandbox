module Zeta.InterpreterSpec where

import           Control.Monad.Writer.Strict
import           Data.Functor.Identity
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Test.Hspec
import           Test.QuickCheck

import           Zeta.Interpreter
import           Zeta.Syntax
import           Zeta.Syntax.Arbitrary       ()

newtype Assignments = Assignments (Map Name [Expr])

instance Monoid Assignments where
  mempty = Assignments mempty
  (Assignments m1) `mappend` (Assignments m2) =
    Assignments $ Map.unionWith (++) m1 m2

newtype TestDomain a = TestDomain
  { runTestDomain :: Writer Assignments a
  } deriving (Functor, Applicative, Monad, MonadWriter Assignments)

instance Domain TestDomain where
  assign name expr = tell $ Assignments [(name, [expr])]

spec :: Spec
spec = do
  pureSpec
  runtimeSpec

pureSpec :: Spec
pureSpec =

  describe "execute without runtime" $ do

    it "fixes literals" $ property $
      \lit -> Literal lit |=> Literal lit

    it "replaces variables by bindings" $ property $
      \name -> Binding [(name, Literal (I 0))] (Var name) |=> Literal (I 0)

    it "returns the last value of a sequence" $
      Sequence (Literal (I 0)) (Literal (I 1)) |=> Literal (I 1)

    it "returns None for assignments" $
      Assignment "x" (Literal (I 0)) |=> Literal None

    it "fails on unbound variables" $
      Var "x" `failsWith` \case NonExistentBinding name -> name == "x"
                                _ -> False

    it "fails on out-of-scope variables" $
      Sequence
        (Binding [("x", Literal (I 0))] (Var "x"))
        (Var "x") `failsWith` \case NonExistentBinding name -> name == "x"
                                    _ -> False

    it "merges nested scopes" $
      Binding [("x", Literal (I 1))] (
        Binding [("y", Literal (I 2))] (
          BinaryOp Plus (Var "x") (Var "y"))) |=> Literal (I 3)

    it "fails on first runtime error" $
      Sequence (Var "x") (Literal (I 0)) `failsWith` \case
        NonExistentBinding name -> name == "x"; _ -> False


  where
    (|=>) source expected =
      let (actual, _) = runWriter $ runTestDomain (execute mempty source)
      in actual `shouldBe` Right expected
    failsWith source errorPredicate =
      let (actual, _) = runWriter $ runTestDomain (execute mempty source)
      in actual `shouldSatisfy` \case
        Left err -> errorPredicate err
        Right _ -> False



runtimeSpec :: Spec
runtimeSpec =

  describe "execute with runtime" $ do

    it "fixes externals" $
      External ["x", "y"] |=> External ["x", "y"]

    it "fails when attempting to apply a missing external" $
      App (External ["x", "y"]) [] `failsWith` \case
        MissingExternal s -> s == (["x", "y"], [])
        _ -> False

    it "invokes external function without arguments" $
      App (External ["zero"]) [] |=> Literal (I 0)

    it "invokes external function with arguments" $
      App (External ["inc"]) [("x", Literal (I 1))] |=> Literal (I 2)

    it "fetches data succesfully" $
      App (Fetch ["weather", "temperature"]) [("city", "Amsterdam")] |=> Literal (I 20)

    it "fetches data within external function" $
      App (External ["kelvin"]) [("city", "Amsterdam")] |=> Literal (I 293)

  where
    env = RuntimeEnv
      { _externals =
          [ ((["zero"], []), Literal (I 0))
          , ((["inc"], ["x"]), BinaryOp Plus (Var "x") (Literal (I 1)))
          , ((["kelvin"], ["city"]),
             BinaryOp Plus (Literal (I 273))
              (App (Fetch ["weather", "temperature"]) [("city", Var "city")]))
          ]
      , _fetch = \urn args -> case (urn, args) of
          (["weather", "temperature"], [("city", "Amsterdam")]) ->
            pure $ Just $ Literal (I 20)
          _ -> pure Nothing
      }

    source |=> expected =
      let (actual, _) = runWriter $ runTestDomain (execute env source)
      in actual `shouldBe` Right expected

    failsWith source errorPredicate =
      let (actual, _) = runWriter $ runTestDomain (execute env source)
      in actual `shouldSatisfy` \case
        Left err -> errorPredicate err
        Right _ -> False
