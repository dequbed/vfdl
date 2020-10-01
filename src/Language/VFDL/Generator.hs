module Language.VFDL.Generator
    ( generateBlueprintJson
    ) where

import Language.VFDL.TLR

import qualified Data.Text.Lazy as TL

import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)

data Tile
    = Tile
    { tileName :: Text
    , tilePosition :: Position
    } deriving (Eq, Show)
instance FromJSON Tile where
    parseJSON = withObject "Tile" $ \v -> Tile
        <$> v .: "name"
        <*> v .: "position"
instance ToJSON Tile where
    toJSON (Tile name position) = object [ "name" .= name, "position" .= position]

data Colour
    = Colour
    { red :: Float
    , green :: Float
    , blue :: Float
    , alpha :: Float
    } deriving (Eq, Ord, Show)
instance FromJSON Colour where
    parseJSON = withObject "Colour" $ \v -> Colour
        <$> v .: "r"
        <*> v .: "g"
        <*> v .: "b"
        <*> v .: "a"
instance ToJSON Colour where
    toJSON (Colour r g b a) = object 
        [ "r" .= r, "g" .= g, "b" .= b, "a" .= a]

data Blueprint
    = Blueprint
    { blueprintItem :: Text
    , blueprintLabel :: Text
    , blueprintLabelColour :: Maybe Colour
    , blueprintEntities :: [Entity]
    , blueprintTiles :: [Tile]
    , blueprintVersion :: Integer
    } deriving (Eq, Show)

instance FromJSON Blueprint where
    parseJSON = withObject "Blueprint" $ \v -> Blueprint
        <$> v .: "item"
        <*> v .: "label"
        <*> v .:? "label_color"
        <*> v .: "entities"
        <*> v .: "tiles"
        <*> v .: "version"
instance ToJSON Blueprint where
    toJSON (Blueprint item label colour entities tiles version) = object
        [ "item" .= item
        , "label" .= label
        , "label_color" .= colour
        , "entities" .= entities
        , "tiles" .= tiles
        , "version" .= version
        ]

generateBlueprintJson :: Text -> [Entity] -> TL.Text
generateBlueprintJson name ents = do
    encodeToLazyText $ Blueprint "blueprint" name Nothing ents [] 1
