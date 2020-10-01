module Language.VFDL.AST
    ( AST
    , PortDirection(..)
    , Port(..)
    , Factory(..)
    ) where

data AST = AST

data PortDirection = In | Out
    deriving (Eq, Show)


data Port = Port
          { portIdentifier :: String
          , portDirection :: PortDirection
          , portType :: String
          } deriving (Eq, Show)

data Factory = Factory
             { facIdentifier :: String
             , facPorts :: Maybe [Port]
             } deriving (Eq, Show)
