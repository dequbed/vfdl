module Language.VFDL.AST
    ( AST(..)
    , PortDirection(..)
    , Port(..)
    , Factory(..)
    , Operation(..)
    , BOp(..)
    , Statement(..)
    , Architecture(..)
    ) where

data AST = AST [Factory] [Architecture]
    deriving (Eq, Show)

instance Semigroup AST where
    (AST f a) <> (AST g b) = AST (f<>g) (a<>b)
instance Monoid AST where
    mempty = AST [] []

data PortDirection = In | Out
    deriving (Eq, Show)


data Port = Port
    { portIdentifier :: Text
    , portDirection :: PortDirection
    , portType :: Text
    } deriving (Eq, Show)

data Factory = Factory
    { facIdentifier :: Text
    , facPorts :: Maybe [Port]
    } deriving (Eq, Show)

data BOp = Merge
    deriving (Eq, Show)

data Operation
    = FunctionCall { name :: Text
                   , parameter :: [Text]
                   }
    | Balance [Text]
    | BeltOperation BOp Text Text
    deriving (Eq, Show)

data Statement = Statement
    { outputs :: [Text]
    , operation :: Operation
    } deriving (Eq, Show)

data Architecture = Architecture
    { archIdentifier :: Text
    , archStatements :: [Statement]
    } deriving (Eq, Show)
