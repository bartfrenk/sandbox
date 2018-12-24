{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import Test.Hspec

import Bitcoin.Internals
import Bitcoin.Arbitrary
import qualified Crypto.PubKey.Ed25519     as PK
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck (modifyMaxSize)


main :: IO ()
main = hspec $ modifyMaxSize (const 100) $ do
  describe "digital signatures" $ do

    it "signed transactions verify under the associated public key" $
      property prop_signAndVerify

  -- TODO: move createTransaction out of the generator
  describe "createTransaction" $ do
    it "creates valid transactions" $ forAll validTxPair $ \(previous, current) ->
      isValidTransaction current previous

prop_signAndVerify :: Transaction -> PK.SecretKey -> Bool
prop_signAndVerify tx sk =
  let pk = PK.toPublic sk
      signature = sign sk (tx :: Transaction)
  in verify pk tx signature


-- test :: IO Bool
-- test = do
--   sk <- PK.generateSecretKey
--   let pk = PK.toPublic sk
--   let txt = ("Hello World!" :: ByteString)
--   let signature = PK.sign sk pk txt
--   pure $ PK.verify pk txt signature

-- test2 :: IO Bool
-- test2 = do
--   sk <- generate arbitrary
--   let pk = PK.toPublic sk
--   tr <- generate arbitrary
--   let signature = sign sk (tr :: Transaction)
--   pure $ verify pk tr signature
