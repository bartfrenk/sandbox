-- TODO: Fill in the undefined functions; use cryptonite
-- (see http://hackage.haskell.org/package/cryptonite-0.25)
{-# LANGUAGE UndecidableInstances       #-}
module Bitcoin.Internals where

import           Control.Monad.Reader
import           Control.Monad.State
import           Crypto.Hash               (Digest, hashFinalize, hashInit,
                                            hashUpdate)
import qualified Crypto.Hash               as H
import           Crypto.Hash.Algorithms    (SHA256 (..))
import           Crypto.PubKey.Ed25519     (PublicKey, SecretKey, Signature)
import qualified Crypto.PubKey.Ed25519     as PK
import           Data.Function             ((&))
import           Data.Monoid
import qualified Data.Set                  as Set
import           Safe                      (headMay)
import           System.Random


type Hash = Digest SHA256

-- |Type class for data that can be (cryptographically) hashed.
class HasHash a where
  hash :: a -> Hash

instance HasHash Transaction where
  hash (Transaction{..}) =
    let ctx = hashInit &
              flip hashUpdate recipient &
              flip hashUpdate signature
    in hashFinalize ctx

instance HasHash (Digest SHA256, PublicKey) where
  hash (digest, pk) =
    let ctx = hashInit &
              flip hashUpdate digest &
              flip hashUpdate pk
    in hashFinalize ctx

instance HasHash (PublicKey) where
  hash = H.hash

-- |In this simple case, a transaction always transfers a complete coin. It has
-- a recipient, identified by its public key, and a spender, which is the one
-- that created the signature using its own private key.
data Transaction = Transaction
  { recipient           :: PublicKey
  , signature           :: Signature
  , prevTransactionHash :: Maybe Hash
  } deriving (Show)

-- |Creates a transaction on top of the previous transaction. If the previous
-- transaction is None, then the transaction creates the coin, for example as a
-- block reward for the miner. Note that if 'sk' does not match the public key
-- recipient in 'previous' the newly created transaction is invalid. This
-- ensures that only the owner of the coin can spend it.
createTransaction :: Maybe Transaction -> SecretKey -> PublicKey -> Transaction
createTransaction (Just previous) sk recipient = Transaction
  { recipient = recipient
  , signature = sign sk (hash previous, recipient)
  , prevTransactionHash = Just $ hash previous
  }
createTransaction Nothing sk recipient = Transaction
  { recipient = recipient
  , signature = sign sk recipient
  , prevTransactionHash = Nothing
  }

-- |Signs a message. Simple wrapper around the ECC sign function.
sign :: HasHash msg => SecretKey -> msg -> Signature
sign sk msg = PK.sign sk (PK.toPublic sk) (hash msg)

-- |Verify the signature. Simple wrapper around the ECC verification function.
verify :: HasHash msg => PublicKey -> msg -> Signature -> Bool
verify pk msg signature = PK.verify pk (hash msg) signature

-- |Check if a new transaction is valid with respect to the supposedly previous
-- transaction of the same coin. This is checking whether the recipient in the
-- previous transaction is the spender of current one.
isValidTransaction :: Transaction -> Transaction -> Bool
isValidTransaction current previous =
  let msg = (hash previous, recipient current)
  in verify (recipient previous) msg (signature current)

-- |An electronic coin is a list of subsequent transactions.
type ElectronicCoin = [Transaction]

-- Proof of work

-- |Type of the nonce to be included in the block.
type Nonce = Int

-- |Gets the final transaction in the chain associated with a specific coin.
-- Returns 'Nothing' if this transaction is the one that created the coin.
getPreviousTransaction :: BlockChain -> Transaction -> Maybe Transaction
getPreviousTransaction chain transaction =
  case prevTransactionHash transaction of
    Nothing -> Nothing
    Just toFind ->
      headMay [candidate | block <- chain, candidate <- txs block,
                           hash candidate == toFind ]

-- |Settings for the blockchain.
data Settings = Settings
  { solvesPuzzle :: BlockChain -> Hash -> Bool -- ^Difficulty of the hash puzzle
  , blockReward  :: Int
  }

-- |Represents a block with or without a nonce.
data Block' a = Block'
  { prevBlockHash :: Maybe Hash
  , nonce         :: a
  , txs           :: [Transaction]
  }

-- |A block containing a nonce, a candidate for inclusion in the chain.
type Block = Block' Nonce

instance HasHash Block where
  hash = undefined

type BlockChain = [Block]

isValidBlockChain :: BlockChain -> Bool
isValidBlockChain [] = True
isValidBlockChain [_] = True
isValidBlockChain (current:previous:rest) =
  if mayExtend current rest
  then isValidBlockChain (previous:rest)
  else False

data BlockChainState
  = Valid
  | InvalidBlockHash Block
  | DoubleSpend Transaction

isValidBlockChain' :: BlockChain -> BlockChainState
isValidBlockChain' chain = recur Nothing Set.empty (reverse chain)
  where
    recur lastBlockHash spendable (b:bs) =
      if lastBlockHash /= prevBlockHash b
      then InvalidBlockHash b
      else undefined

mayExtend :: Block' a -> BlockChain -> Bool
mayExtend _ [] = True
mayExtend candidate chain@(current:_) =
  if Just (hash current) == prevBlockHash candidate
  then getAll (checkTransaction `foldMap` txs candidate)
  else False
  where checkTransaction tx = All $ mayTransact chain tx

-- |Checks if 'tx' is a valid transaction given the current state as represented
-- in 'chain'.
mayTransact :: BlockChain -> Transaction -> Bool
mayTransact chain tx =
  case getPreviousTransaction chain tx of
    Just previous -> isValidTransaction tx previous
    Nothing -> True

createBlock :: BlockChain -> [Transaction] -> Block' ()
createBlock chain txs = Block'
  { prevBlockHash = Just $ hash $ last chain
  , nonce = ()
  , txs = txs
  }

-- |The monad for miners, allows for custom mining settings (e.g. the
-- difficulty), and for generating randomness.
type MineM = ReaderT Settings (State StdGen)

-- |Convenience function to generate a random nonce in the `MineM` monad.
randomNonce :: MineM Nonce
randomNonce = do
  s <- get
  let (nonce, s') = random s
  put s'
  pure nonce

-- |Takes a block and computes a nonce for that block that solves the
-- parametrized hash puzzle.
mine :: Block' () -> BlockChain -> MineM Block
mine block chain = loop
  where
    loop = do
      candidate <- randomNonce
      check <- reader solvesPuzzle
      if check chain (hash block { nonce = candidate })
        then pure $ block { nonce = candidate }
        else loop


-- Reclaiming disk space

data BinaryTree a = Tip a | Branch (BinaryTree a) (BinaryTree a)

data MerkleTree a = MerkleTree (Hash, BinaryTree a)

type TransactionTree = MerkleTree Transaction

compactMerkleTree :: MerkleTree a -> (a -> Bool) -> MerkleTree (Either a Hash)
compactMerkleTree = undefined

