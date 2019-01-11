
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
import           Test.Hspec

import           Bitcoin
import           Bitcoin.Arbitrary
import qualified Bitcoin.Internals          as I
import qualified Crypto.PubKey.Ed25519      as PK
import           Test.Hspec.Core.QuickCheck (modifyMaxSize)
import           Test.QuickCheck


main :: IO ()
main = hspec $ modifyMaxSize (const 100) $ do
  describe "sign and verify" $

    it "verifies transactions signed with the correct public key" $
      property $ \tx sk ->
      let pk = PK.toPublic sk
          signature = I.sign sk (tx :: Transaction)
      in I.verify pk tx signature

  describe "createTransaction and isValidTransaction" $

    it "creates valid transactions" $ property $ \(secretKey, publicKey) ->
      forAll (transactionTo publicKey) $ \prevTx -> property $ \recipient ->
      isValidTransaction (createTransaction (Just prevTx) secretKey recipient)

  describe "mine" $

    it "generates a nonce that solves the hash puzzle" pending

  describe "isValidBlockChain" $ do

    -- The generator for secret key-public key pairs generates an actual
    -- asymmetric key pair.
    it "detects double spending" $ property $ \(secretKey, publicKey) ->
      forAll (transactionTo publicKey) $ \tx ->
      forAll (listOf $ transactionFrom (Just tx) secretKey) $ \spends ->
      forAll (blockChainWithTransactions (tx:spends)) $ \chain ->
      isValidBlockChain chain `shouldBe` DoubleSpend tx

    -- Since the generators for public keys and secret keys are independent the
    -- generated keys should be astronomically unlikely to form a pair.
    it "detects invalid signatures" $ property $ \publicKey secretKey ->
      forAll (transactionTo publicKey) $ \tx ->
      forAll (transactionFrom (Just tx) secretKey) $ \spendTx ->
      forAll (blockChainWithTransactions [tx, spendTx]) $ \chain ->
      isValidBlockChain chain `shouldBe` InvalidSignature spendTx
