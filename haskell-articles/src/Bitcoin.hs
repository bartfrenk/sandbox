-- TODO: Fill in the undefined functions; use cryptonite
-- (see http://hackage.haskell.org/package/cryptonite-0.25)
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Bitcoin where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString
import           Data.Monoid
import           System.Random

-- Transactions

data PublicKey

data PrivateKey

data Signature

newtype Hash = Hash ByteString deriving (Eq)

-- |Type class for data that can be hashed.
class Hashable a where
  hash :: a -> Hash

instance Hashable Transaction where
  hash = undefined

instance Hashable (Hash, PublicKey) where
  hash = undefined

-- |In this simple case, a transaction always transfers a complete coin. It has
-- a recipient, identified by its public key, and a spender, which is the one
-- that created the signature using its own private key.
data Transaction = Transaction
  { recipient :: PublicKey
  , signature :: Signature
  }

-- |Check if a new transaction is valid with respect to the supposedly previous
-- transaction of the same coin. This is checking whether the recipient in the
-- previous transaction is the spender of current one.
isValidTransaction :: Transaction -> Transaction -> Bool
isValidTransaction current previous =
  let h = hash (hash previous, recipient current)
  in verify (recipient previous) h (signature current)

-- |Verify the signature.
verify :: PublicKey -> Hash -> Signature -> Bool
verify = undefined

-- |An electronic coin is a list of subsequent transactions.
type ElectronicCoin = [Transaction]

-- |Check if an electronic coin is valid by checking that every transaction in
-- the chain was signed by the owner of the coin after the previous transaction.
isValidCoin :: ElectronicCoin -> Bool
isValidCoin [] = True
isValidCoin [_] = True
isValidCoin (current:previous:rest) =
  if isValidTransaction current previous
  then isValidCoin (previous:rest)
  else False

-- Proof of work

-- |Type of the nonce to be included in the block.
type Nonce = Int

-- |Gets the final transaction in the chain associated with a specific coin.
getPreviousTransaction :: BlockChain -> Transaction -> Transaction
getPreviousTransaction = undefined

-- |Settings for the blockchain.
data Settings = Settings
  { solvesPuzzle :: BlockChain -> Hash -> Bool -- ^Difficulty of the hash puzzle
  }

-- |Represents a block with or without a nonce.
data Block' a = Block'
  { prevHash :: Hash
  , nonce    :: a
  , txs      :: [Transaction]
  }

-- |A block containing a nonce, a candidate for inclusion in the chain.
type Block = Block' Nonce

instance Hashable Block where
  hash = undefined

type BlockChain = [Block]

isValidBlockChain :: BlockChain -> Bool
isValidBlockChain [] = True
isValidBlockChain [_] = True
isValidBlockChain (current:previous:rest) =
  if mayExtend current rest
  then isValidBlockChain (previous:rest)
  else False

mayExtend :: Block' a -> BlockChain -> Bool
mayExtend _ [] = True
mayExtend candidate chain@(current:_) =
  if hash current == prevHash candidate
  then getAll (checkTransaction `foldMap` txs candidate)
  else False
  where checkTransaction tx = All $ mayTransact chain tx

-- |
mayTransact :: BlockChain -> Transaction -> Bool
mayTransact chain tx =
  let previous = getPreviousTransaction chain tx
  in isValidTransaction tx previous

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


