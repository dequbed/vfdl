module Language.VFDL.Compile
    ( compile
    ) where

import Language.VFDL.AST

import Data.Graph

import qualified Data.IntMap as M

import Data.HashMap.Strict

data Entity = Entity
    { name :: Text
    } deriving (Eq, Ord, Show)

data Internal k = Internal
    { internalNetlist :: Graph
    , internalVLookup :: Vertex -> (Entity, k, [k])
    , internalLookup :: k -> Maybe Vertex
    , internalSymbols :: HashMap Entity k
    }

compile :: AST -> [()]
compile (AST [] []) = []
compile (AST fs as) = []
    
