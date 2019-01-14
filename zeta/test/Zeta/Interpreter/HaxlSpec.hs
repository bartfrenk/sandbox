module Zeta.Interpreter.HaxlSpec where

import Test.Hspec

import Zeta.Syntax

import qualified Zeta.Interpreter.Haxl.MockRequest as Mock

spec :: Spec
spec =
  describe "Haxl interpreter" $ do
    it "evaluates Fetch terms" $
      Fetch ["city"] [("ip-address", "8.8.8.8")] |=> "Mountain View"

    it "returns None when Fetch term does not match a request" $
      Fetch ["temperature"] [] |=> Literal None

    it "allows for fetching arguments of fetches" $
      Fetch ["city"][("ip-address", Fetch ["ip-address"] [])] |=> "Berkeley"
  where
    fetches =
      [ ((["city"], [("ip-address", "8.8.8.8")]), "Mountain View")
      , ((["city"], [("ip-address", "9.9.9.9")]), "Berkeley")
      , ((["ip-address"], []), "9.9.9.9")
      ]
    (|=>) :: Expr -> Expr -> Expectation
    (|=>) source expected =
      let actual = fst <$> Mock.execute [] fetches source
      in actual `shouldReturn` Right expected


