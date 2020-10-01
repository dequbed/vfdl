module Language.VFDL.AST
    ( AST
    , PortDirection(..)
    , Port(..)
    , Factory(..)
    , Operation(..)
    , Statement(..)
    , Architecture(..)
    ) where

data AST = AST

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

data Operation
    = FunctionCall { name :: Text
                   , parameter :: [Text]
                   }
    | Split Text Text
    | Combine Text Text
    deriving (Eq, Show)

data Statement = Statement
    { output :: Text
    , operation :: Operation
    } deriving (Eq, Show)

data Architecture = Architecture
    { archIdentifier :: Text
    , archStatements :: [Statement]
    } deriving (Eq, Show)
