{-# LANGUAGE TemplateHaskell #-}
module Zeta.Interpreter.Haxl.Resource where

import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Map.Strict   (Map, (!?))
import qualified Data.Map.Strict   as Map
import           Data.Set
import           Data.String
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Data.Validation
import           GHC.Generics      (Generic)
import           Lens.Micro.Extras

import           Zeta.Syntax
import           Zeta.Types

-- TODO: This probably needs to move to the type checker when it exists.
data ContextType = TyString | TyBool | TyInteger
  deriving (Eq, Ord, Show)

instance FromJSON ContextType where
  parseJSON = withText "ContextType" $ \t ->
    case t of
      "string" -> pure TyString
      "bool" -> pure TyBool
      "integer" -> pure TyInteger
      _ -> fail $ "invalid type " ++ T.unpack t

newtype Selector a = Selector (a -> Maybe Expr)

-- |Intermediate represenation of a 'Selector' that is easier for parsing and
-- printing.
data SelectorDescription = SelectorDescription
  { id          :: URN
  , path        :: [Text]
  , contextType :: ContextType
  } deriving (Eq, Show)

instance FromJSON SelectorDescription where
  parseJSON = withObject "ContextSelector" $ \obj ->
    SelectorDescription <$> obj .: "id" <*> obj .: "path" <*> obj .: "type"

makeSelector :: ToJSON a => SelectorDescription -> Selector a
makeSelector SelectorDescription{..} = Selector $ \repr ->
    case contextType of
      TyString -> select path (Literal . S) _String (toJSON repr)
      TyBool -> select path (Literal . B) _Bool (toJSON repr)
      TyInteger -> select path (Literal . I . fromIntegral) _Integer (toJSON repr)
    where
      select path make ty val =
        let getter = foldl1 (.) (key `fmap` path)
        in make <$> preview (getter . ty) val

data ResourceDescription = ResourceDescription
  { uriTemplate :: TemplateString Name
  , contexts    :: [SelectorDescription]
  } deriving (Eq, Show, Generic)

instance FromJSON ResourceDescription where
  parseJSON = withObject "ResourceDescription" $ \obj ->
    ResourceDescription <$> obj .: "uri" <*> obj .: "contexts"

newtype URI = URI Text

data Resource a = Resource
  { makeUri   :: Map Name Literal -> Validation [Name] URI
  , selectors :: Map URN (Selector a)
  , signature :: Set Name
  }

makeFetches :: Resource a -> Map (URN, Set Name) (Resource a)
makeFetches res = undefined

makeResource :: ToJSON a => ResourceDescription -> Resource a
makeResource ResourceDescription{..} =
  Resource
  { makeUri = fmap URI . asFunction uriTemplate
  , selectors = Map.fromList (fromSelectorDescription <$> contexts)
  , signature = placeholders uriTemplate
  }
  where

    fromSelectorDescription desc@SelectorDescription{..} = (id, makeSelector desc)

-- test :: IO (Either ParseException Resource)
-- test = decodeFileEither "docs/resource.yaml"

-- ipstack' :: Resource
-- ipstack' = Resource
--   { url = const "http://api.ipstack.com/82.171.74.76?access_key=2b5b300141b464d17f33178463bc2456"
--   , contexts = [(["city"],  fmap (Literal . S) . preview (key "city" . _String))]
--   }



