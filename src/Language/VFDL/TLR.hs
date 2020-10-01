module Language.VFDL.TLR
    ( Entity(..)
    , EntityType(..)
    , Position(..)
    ) where

import Data.Text (unpack)
import Data.Aeson

-- Position of an Entity or Tile relative to the middle of the blueprint
data Position = Position Float Float
    deriving (Eq, Ord, Show)

instance FromJSON Position where
    parseJSON = withObject "Position" $ \v -> Position
        <$> v .: "x"
        <*> v .: "y"
instance ToJSON Position where
    toJSON (Position x y) = object [ "x" .= x, "y" .= y ]

data EntityType = Input | Output
    deriving (Eq, Show)

instance FromJSON EntityType where
    parseJSON = withText "Entity Type" $ \case
        "input" -> return Input
        "output" -> return Output
        n -> fail $ unpack $ (n <> " is not a valid entity type")
instance ToJSON EntityType where
    toJSON Input = "input"
    toJSON Output = "output"

data Entity 
    = Entity
    { entityNumber :: Integer
    , entityName :: Text
    , entityPosition :: Position
    , entityDirection :: Maybe Integer
    , entityRecipe :: Maybe Text
    , entityType :: Maybe EntityType
    , entityDropPosition :: Maybe Position
    , entityPickupPosition :: Maybe Position
    } deriving (Eq, Show)

instance FromJSON Entity where
    parseJSON = withObject "Entity" $ \v -> Entity
        <$> v .: "entity_number"
        <*> v .: "name"
        <*> v .: "position"
        <*> v .:? "direction"
        <*> v .:? "recipe"
        <*> v .:? "type"
        <*> v .:? "drop_position"
        <*> v .:? "pickup_position"
instance ToJSON Entity where
    toJSON (Entity num name pos dir recipe t droppos pickpos) = object
        [ "entity_number" .= num
        , "name" .= name
        , "position" .= pos
        , "direction" .= dir
        , "recipe" .= recipe
        , "type" .= t
        , "drop_position" .= droppos
        , "pickup_position" .= pickpos
        ]
