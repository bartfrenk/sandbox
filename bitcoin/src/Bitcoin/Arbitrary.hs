{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bitcoin.Arbitrary where

import           Bitcoin.Internals
import qualified Crypto.Hash               as H
import qualified Crypto.PubKey.Ed25519     as PK
import           Crypto.Random.Types       (MonadRandom (..))
import qualified Data.ByteArray            as BA
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import Crypto.Error
import Data.ByteString as ByteString


instance MonadRandom Gen where
    getRandomBytes size = BA.take size <$> BA.pack <$> listOf arbitrary

instance Arbitrary PK.SecretKey where
  -- Only return when we've found a byte string that maps to a private key.
  arbitrary = do
    bs <- BA.pack <$> vectorOf 32 arbitrary
    case PK.secretKey (bs :: ByteString) of
      CryptoPassed sk -> pure sk
      _ -> arbitrary


instance Arbitrary PK.PublicKey where
  arbitrary = PK.toPublic <$> arbitrary

instance H.HashAlgorithm a => Arbitrary (H.Digest a) where
  arbitrary = do
    size <- arbitrary
    bytes <- (getRandomBytes size :: Gen BA.Bytes)
    pure $ H.hash bytes

instance Arbitrary Transaction where
  arbitrary = do
    recipient <- arbitrary
    prevHash <- arbitrary
    sk <- arbitrary
    let signature = sign sk (prevHash, recipient)
    pure $ Transaction recipient signature (Just prevHash)

validTxPair :: Gen (Transaction, Transaction)
validTxPair = do
  sk <- arbitrary
  let recipient = PK.toPublic sk
  prevHash <- arbitrary
  let previous = Transaction recipient (sign sk (prevHash, recipient)) (Just prevHash)
  current <- createTransaction (Just previous) sk <$> arbitrary
  pure $ (previous, current)
