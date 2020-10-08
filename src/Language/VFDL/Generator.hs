module Language.VFDL.Generator
    ( generateBlueprintJSON
    , generateBlueprint
    ) where

import Language.VFDL.TLR

import qualified Data.Text.Lazy as TL

import qualified Data.ByteString.Lazy as BL

import Data.Aeson

import Codec.Compression.Zlib (compress)
import Data.ByteString.Lazy.Base64 (encodeBase64)

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
        [("blueprint", object $
            [ "item" .= item
            , "label" .= label
            , "entities" .= entities
            , "tiles" .= tiles
            , "version" .= version
            ] ++ (case colour of
                Just c -> ["label_color" .= c]
                Nothing -> [])
         )]

-- |Generate the uncompressed unencoded JSON. This is not a valid blueprint as is but
-- human-readable
generateBlueprintJSON :: Text -> [Entity] -> BL.ByteString
generateBlueprintJSON name ents = do
    encode $ Blueprint "blueprint" name Nothing ents [] 1

-- |Generate the final blueprint string ready to be written to a file
--  This means the JSON is compressed using zlib-deflate, encoded as Base64 and prefixed
--  with the version '0'
generateBlueprint :: Text -> [Entity] -> TL.Text
generateBlueprint n e = "0" <> (encodeBase64 $ compress $ generateBlueprintJSON n e)
