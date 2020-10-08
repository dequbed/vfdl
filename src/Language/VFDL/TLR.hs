module Language.VFDL.TLR
    ( Entity(..)
    , EntityType(..)
    , Position(..)
    ) where

import Data.Text (unpack)
import Data.Aeson

-- Tile level representation
-- at this abstraction level we already know exactly what tiles are going to an entity on top
-- of them and which one it's going to be.

-- Position of an Entity or Tile relative to the middle of the blueprint
data Position = Position Float Float
    deriving (Eq, Ord, Show)

-- Used for entities that can be different types such as underground belts or loaders
data EntityType = Input | Output
    deriving (Eq, Show)

-- An Entity is the main object in a blueprint, providing all the functionality
data Entity 
    = Entity
    { entityNumber :: Int
    , entityName :: Text
    , entityPosition :: Position
    , entityDirection :: Maybe Integer
    , entityRecipe :: Maybe Text
    , entityType :: Maybe EntityType
    , entityDropPosition :: Maybe Position
    , entityPickupPosition :: Maybe Position
    } deriving (Eq, Show)


instance FromJSON EntityType where
    parseJSON = withText "Entity Type" $ \case
        "input" -> return Input
        "output" -> return Output
        n -> fail $ unpack $ (n <> " is not a valid entity type")
instance ToJSON EntityType where
    toJSON Input = "input"
    toJSON Output = "output"

instance FromJSON Position where
    parseJSON = withObject "Position" $ \v -> Position
        <$> v .: "x"
        <*> v .: "y"
instance ToJSON Position where
    toJSON (Position x y) = object [ "x" .= x, "y" .= y ]


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
    toJSON (Entity num name pos dir recipe t droppos pickpos) = object $
        [ "entity_number" .= num
        , "name" .= name
        , "position" .= pos
        ] ++
        case dir of 
          Just d -> ["direction" .= d]
          Nothing -> []
        ++
        case recipe of
          Just r -> ["recipe" .= recipe]
          Nothing -> []
        ++
        case t of
          Just t -> ["type" .= t]
          Nothing -> []
        ++
        case droppos of
          Just dp -> ["drop_position" .= droppos]
          Nothing -> []
        ++
        case pickpos of
          Just pp -> ["pickup_position" .= pickpos]
          Nothing -> []
