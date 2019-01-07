module Zeta.ParserSpec where

import Test.Hspec

import Data.Either (isLeft)
import Data.Text as T
import Zeta.Parser
import Zeta.Syntax

spec :: Spec
spec = do

  describe "parse" $ do

    it "assignments" $
      parse "x = 1" `shouldBe` Right [Assignment "x" (Literal (I 1))]

    it "let bindings" $
      parse "let x = 1" `shouldBe` Right [Binding "x" (Literal (I 1))]

    it "multiple statements" $
      let txt = T.unlines
            [ "x = 1"
            , "y = 2"
            , "let z = 3"
            ]
      in parse txt `shouldBe` Right
         [ Assignment "x" (Literal (I 1))
         , Assignment "y" (Literal (I 2))
         , Binding "z" (Literal (I 3))
         ]

    it "equality comparison" $
      parse "x = 3 == 4" `shouldBe` Right
      [ Assignment "x" (BinaryOp OpEQ (Literal (I 3)) (Literal (I 4))) ]

    it "left associativity" $
      parse "x = 3 == 4 == 5" `shouldBe` Right
      [ Assignment "x" (BinaryOp OpEQ
                         (BinaryOp OpEQ (Literal (I 3)) (Literal (I 4)))
                         (Literal (I 5)))
      ]

    it "variable" $
      parse "x = y" `shouldBe` Right
      [ Assignment "x" (Var "y")
      ]

    it "resolver with valid URN" $
      parse "let x = resolver(urn:context:x)" `shouldBe` Right
      [ Binding "x" (Resolver ["context", "x"]) ]

    it "resolver without URN" $
      parse "let x = resolver()" `shouldSatisfy` isLeft

    it "resolver with empty URN" $
      parse "let x = resolver(urn:)" `shouldSatisfy` isLeft
