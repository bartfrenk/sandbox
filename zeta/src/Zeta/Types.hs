module Zeta.Types where

import           Data.Aeson    (FromJSON(..), withText)
import           Data.Hashable (Hashable)
import           Data.Text     (Text)
import           GHC.Exts      (IsList (..))
import qualified Data.Text as T

newtype URN = URN [Text] deriving (Eq, Ord, Show, Hashable)

instance IsList URN where
  type Item URN = Text
  fromList = URN
  toList (URN ts) = ts

instance FromJSON URN where
  parseJSON = withText "URN" $ \t ->
    let segments = T.split (`elem` (":" :: String)) t
    in case segments of
      ("urn":rest) -> pure $ URN rest
      _ -> fail "URN needs to start with 'urn:`"



