module Zeta.InterpreterSpec where

import           Data.Either
import           Data.Functor.Identity
import           Test.Hspec

import           Zeta.Interpreter
import           Zeta.Syntax

spec :: Spec
spec = do

  describe "interpret" $ do

    it "fixes a program with a single assignment" $
      let ast = [Assignment "x" (Literal $ I 1)]
      in runIdentity (interpret ast) `shouldBe` Right ast

    it "allows equality comparison of integers" $
      let ast = [Assignment "x"
                  (BinaryOp OpEQ (Literal $ I 1) (Literal $ I 2))]
      in runIdentity (interpret ast) `shouldBe` Right
         [Assignment "x" (Literal $ B False)]

    it "fails on inequality comparison of booleans" $
      let ast = [Assignment "x"
                  (BinaryOp OpGE (Literal $ B True) (Literal $ B False))]
      in runIdentity (interpret ast) `shouldSatisfy` isLeft

    it "fails on duplicate bindings" $
      let ast =
            [ Binding "x" (Literal $ I 1)
            , Binding "x" (Literal $ I 1)
            ]
      in runIdentity (interpret ast) `shouldSatisfy` isLeft

    it "fails on unbound variables" $
      let ast = [ Assignment "x" (Var "y") ]
      in runIdentity (interpret ast) `shouldSatisfy` isLeft

    it "replaces bound variable by the bound expression" $
      let ast = [ Binding "x" (Literal $ I 1)
                , Assignment "y" (Var "x")
                ]
      in runIdentity (interpret ast) `shouldBe` Right
         [Assignment "y" (Literal $ I 1)]
