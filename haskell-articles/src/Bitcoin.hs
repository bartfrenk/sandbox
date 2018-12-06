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

class Hashable a where
  hash :: a -> Hash

instance Hashable Transaction where
  hash = undefined

instance Hashable (Hash, PublicKey) where
  hash = undefined

data Transaction = Transaction
  { recipient :: PublicKey
  , signature :: Signature
  }

isValidTransaction :: Transaction -> Transaction -> Bool
isValidTransaction current previous =
  let h = hash (hash previous, recipient current)
  in verify (recipient previous) h (signature current)

verify :: PublicKey -> Hash -> Signature -> Bool
verify = undefined

-- The head of the list is the last transaction
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

type Nonce = Int

getPreviousTransaction :: BlockChain -> Transaction -> Transaction
getPreviousTransaction = undefined

data Settings = Settings
  { solvesPuzzle :: BlockChain -> Hash -> Bool -- ^Difficulty of the hash puzzle
  }

-- |Represents a block in with or without a nonce.
data Block' a = Block'
  { prevHash :: Hash
  , nonce    :: a
  , txs      :: [Transaction]
  }

-- |A block containing a nonce.
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

mayTransact :: BlockChain -> Transaction -> Bool
mayTransact chain tx =
  let previous = getPreviousTransaction chain tx
  in isValidTransaction tx previous

type MineM = ReaderT Settings (State StdGen)

randomNonce :: MineM Nonce
randomNonce = do
  s <- get
  let (nonce, s') = random s
  put s'
  pure nonce

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


