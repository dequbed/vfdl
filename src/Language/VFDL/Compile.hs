{-# LANGUAGE DeriveAnyClass #-}
module Language.VFDL.Compile
    ( compile
    , testGraph
    ) where

import qualified Prelude.List as L

import System.IO (hPutStrLn)

import Language.VFDL.AST

import Data.Graph

import qualified Data.IntMap as M
import qualified Data.HashMap.Strict as Map

data NetIdent
    = IdentEntity Int
    | IdentVirtual Identifier
    deriving (Eq, Generic, Hashable, Ord, Show)

data Network
    = Virtual
    | Entity Statement
    deriving (Eq, Show)

type Key = NetIdent
type Node = Network

data Internal = Internal
    { i_netgraph :: Graph
    , i_lookup :: (Key -> Maybe (Node, Key, [Key]))
    }

compile :: AST -> IO ()
--compile (AST [] []) = Nothing
compile (AST fs as) = do
    mapM_ compileOne as

compileOne :: ArchitectureBody -> IO ()
compileOne (ArchitectureBody ident name decls statems) = do
        -- Insert all Statements into a enumerated List
    let identmap = foldl' (flip $ uncurry M.insert) M.empty $ zip [1..] statems
        identmap :: IntMap ConcurrentStatement

        -- Store all transport forwards — i.e. items going from A -> {B,..}
        fwmap = foldl' insertNets Map.empty (M.toList identmap)
        fwmap :: HashMap NetIdent [NetIdent]

        edges = map (\(a, (b,c)) -> (a,b,c)) $ zip (L.repeat ()) (Map.toList fwmap)
        (g,a,b) = graphFromEdges edges
    (hPutStrLn stdout . show) identmap
    (hPutStrLn stdout . show) g
    (hPutStrLn stdout . show) fwmap

getDeps :: Statement -> [Identifier]
getDeps (CSProcessCall (ProcessCall _ params)) = catMaybes $ map unquotedM params
  where
    unquotedM (Unquoted i) = Just i
    unquotedM _ = Nothing
getDeps (CSBalance i) = i
getDeps (CSOperator (Merge a b)) = [a,b]

insertNets :: HashMap NetIdent [NetIdent] -> (Int, ConcurrentStatement) -> HashMap NetIdent [NetIdent]
insertNets m (i, (ConcurrentStatement targets s)) = do
    let entident = IdentEntity i -- the identifier for this entity
        vtargets = map IdentVirtual targets -- Make `targets` into NetIdents

        -- First insert Entity -> {Targets} into the graph
        m2 = Map.insertWith (<>) entident vtargets m
        -- Then insert {Params} -> Entity  into the graph
        m3 = insertEntParam m2 entident s

    m3
  where
    insertEntParam :: HashMap NetIdent [NetIdent] -> NetIdent -> Statement -> HashMap NetIdent [NetIdent]
    insertEntParam netMap entident s = foldl' -- Insert all the Params:
            -- fold function, collecting. Since a parameter can be added to many processes, combine (<>)
            (\m d -> Map.insertWith (<>) d [entident] m)
            netMap $
                -- Make the Parameters NetIdent's instead of Identifier
                map IdentVirtual 
                    -- Extract the unquoted identifier from the parameter list
                    $ getDeps s
        --foldl' (\m p -> Map.insert p [entident]) m params

testGraph = graphFromEdges 
    [ ("copper", 1, [2,3,4])
    , ("craft1", 2, [6])
    , ("craft2", 3, [6,7])
    , ("craft3", 4, [7])
    , ("iron", 5, [6,7])
    , ("craft4", 6, [8])
    , ("craft5", 7, [8])
    , ("Balance1", 8, [])
    ]
