{-# OPTIONS_GHC -fno-warn-orphans #-}
module Zeta.Syntax.Arbitrary where

import qualified Data.Text             as T
import           Test.QuickCheck

import           Zeta.Syntax.Internals

instance Arbitrary Literal where
  arbitrary = oneof [I <$> arbitrary, B <$> arbitrary, pure None]

instance Arbitrary Name where
  arbitrary = Name . T.pack <$> arbitrary



