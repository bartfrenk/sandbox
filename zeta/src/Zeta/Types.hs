module Zeta.Types
  ( TemplateString
  , fromString
  , toTemplateString
  , placeholders
  , asFunction
  , TemplateParam(..)
  , URN(..)
  ) where

import Data.Validation
import           Data.Aeson      (FromJSON (..), withText)
import           Data.Function   ((&))
import           Data.Hashable   (Hashable)
import           Data.Map        ((!?))
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Semigroup
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.String
import           Data.Text       (Text)
import qualified Data.Text       as T
import           GHC.Exts        (IsList (..))
import           Text.Parsec

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

-- |A template string, where placeholders are indicated by wrapping them in
-- curly braces.
newtype TemplateString p = TemplateString [Segment p]
  deriving (Eq)

instance Show p => Show (TemplateString p) where
  show (TemplateString segments) = T.unpack $ T.concat $ show' <$> segments
    where show' (Placeholder txt) = "{" <> T.pack (show txt) <> "}"
          show' (Literal txt) = txt

instance IsString p => FromJSON (TemplateString p) where
  parseJSON = withText "TemplateString" $ \t ->
    case toTemplateString t of
      Left _ -> fail "not a valid template string"
      Right ts -> pure ts

-- |Allow interpretation of string literals as template string. Note that this
-- might fail, for example '{}' is not a valid template string.
instance IsString p => IsString (TemplateString p) where
  fromString s = case toTemplateString (T.pack s) of
    Left _ -> error "not a valid template string"
    Right ts -> ts

data Segment a = Placeholder a | Literal Text
  deriving (Eq)

-- |Creates a template string. Prefer this one over 'fromString' for explicit
-- failures.
toTemplateString :: IsString p => Text -> Either ParseError (TemplateString p)
toTemplateString = runParser parser () ""
  where
    parser = TemplateString <$> many (placeholder <|> literal)
    literal = Literal . T.pack <$> many1 (noneOf "{}")
    placeholder = Placeholder . fromString <$>
      (char '{' *> many1 (noneOf "}") <* char '}')

-- |Extracts the placeholder names from the template string.
placeholders :: Ord p => TemplateString p -> Set p
placeholders (TemplateString segments) =
  segments &
  fmap (\case Placeholder txt -> Just txt; _ -> Nothing) &
  catMaybes &
  Set.fromList

-- |Makes the template string into a function. The placeholder names are the
-- keys of the argument map.
asFunction :: (TemplateParam a, Ord p)
           => TemplateString p -> Map p a -> Validation [p] Text
asFunction (TemplateString segments) args =
  T.concat <$> (interpolate `traverse` segments)
  where
    interpolate (Literal txt) = Success txt
    interpolate (Placeholder name) = case args !? name of
      Just txt -> maybe (Failure [name]) Success (asParam txt)
      Nothing -> Failure [name]

class TemplateParam a where
  asParam :: a -> Maybe Text

