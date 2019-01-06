module Zeta.ParserSpec where

import Test.Hspec

import Data.Text as T
import Zeta.Parser
import Zeta.Syntax

spec :: Spec
spec = do

  describe "parse" $ do

    it "assignments" $
      parse "x = 1" `shouldBe` Right [Assignment "x" (IntLiteral 1)]

    it "let bindings" $
      parse "let x = 1" `shouldBe` Right [Binding "x" (IntLiteral 1)]

    it "multiple statements" $
      let txt = T.unlines
            [ "x = 1"
            , "y = 2"
            , "let z = 3"
            ]
      in parse txt `shouldBe` Right
         [ Assignment "x" (IntLiteral 1)
         , Assignment "y" (IntLiteral 2)
         , Binding "z" (IntLiteral 3)
         ]

    it "equality comparison" $
      parse "x = 3 == 4" `shouldBe` Right
      [ Assignment "x" (BinaryOp OpEQ (IntLiteral 3) (IntLiteral 4)) ]

    it "left associativity" $
      parse "x = 3 == 4 == 5" `shouldBe` Right
      [ Assignment "x" (BinaryOp OpEQ
                         (BinaryOp OpEQ (IntLiteral 3) (IntLiteral 4))
                         (IntLiteral 5))
      ]

