{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bitcoin.Arbitrary where

import           Bitcoin.Internals
import           Control.Monad
import           Crypto.Error
import qualified Crypto.Hash               as H
import qualified Crypto.PubKey.Ed25519     as PK
import           Crypto.Random.Types       (MonadRandom (..))
import qualified Data.ByteArray            as BA
import           Data.ByteString           (ByteString)
import           Data.List                 (foldl')
import           Test.QuickCheck.Arbitrary
import System.Random (mkStdGen)
import           Test.QuickCheck.Gen


instance MonadRandom Gen where
    getRandomBytes size = BA.take size <$> BA.pack <$> listOf arbitrary

instance Arbitrary PK.SecretKey where
  -- Only return when we've found a byte string that maps to a private key.
  arbitrary = do
    bs <- BA.pack <$> vectorOf PK.secretKeySize arbitrary
    case PK.secretKey (bs :: ByteString) of
      CryptoPassed sk -> pure sk
      _ -> arbitrary

instance Arbitrary PK.Signature where
  arbitrary = do
    bs <- BA.pack <$> vectorOf PK.signatureSize arbitrary
    case PK.signature (bs :: ByteString) of
      CryptoPassed signature -> pure signature
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

-- |Arbitrary transaction with destination account indicated by 'recipient'.
transactionTo :: PK.PublicKey -> Gen Transaction
transactionTo recipient =
  Transaction recipient <$> arbitrary <*> arbitrary

-- |Creates a new transaction on top of 'mtx'. Creates a coin generating
-- transaction if 'mtx' is Nothing.
transactionFrom :: Maybe Transaction -> PK.SecretKey -> Gen Transaction
transactionFrom mtx sk = createTransaction mtx sk <$> arbitrary

-- |Creates a mined block containing only the specified transactions. The hash
-- puzzle used for mining is trivial.
blockWithTransactions :: BlockChain -> [Transaction] -> Block
blockWithTransactions chain txs =
  let settings = Settings
        { solvesPuzzle = \_ _ -> True
        , blockReward = 0
        }
  in runMineT settings (mkStdGen 0) $
     mine (createBlock chain txs) chain

-- |Creates an otherwise valid chain of block that contains the list of
-- transactions in the order in which they appear in the list.
blockChainWithTransactions :: [Transaction] -> Gen BlockChain
blockChainWithTransactions txs = do
  partition <- listPartition txs
  pure $ foldl' recur [] partition

  where

    recur :: BlockChain -> [Transaction] -> BlockChain
    recur [] txs' = [blockWithTransactions [] txs']
    recur chain txs' =
      let nextBlock = blockWithTransactions chain txs'
      in nextBlock:chain

-- |Generates a partition into consecutive sublists of a list.
listPartition :: [a] -> Gen [[a]]
listPartition xs = do
  (n, split) <- liftM2 (,) (choose (0, (length xs - 1))) chooseAny
  if split
    then (take n xs:) <$> listPartition (drop n xs)
    else pure [xs]
