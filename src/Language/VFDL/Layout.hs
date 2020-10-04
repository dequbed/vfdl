{-# LANGUAGE DeriveAnyClass #-}
module Language.VFDL.Layout
    ( testLayout
    ) where

import System.IO

import Language.VFDL.Compile

import Data.Tree
import qualified Data.HashMap.Strict as Map

data Tile = Tile (Int, Int)
    deriving (Eq, Show)

data Direction = In | Out
    deriving (Eq, Show, Generic, Hashable)
type Port = (Text, Direction)

data PortKind = Belt | Grabber
    deriving (Eq, Show)

data Layout = Layout 
    { layout_dimensions :: (Int, Int) -- ^ Bounding box. TODO: Make this not a box but any shape
    , layout_io_blocks :: HashMap Port (PortKind, [Tile])
    } deriving (Eq, Show)

-- |Merge two layouts if a solution could be found
mergeLayouts :: Layout -> Layout -> Maybe Layout
mergeLayouts (Layout _ a) (Layout _ b) = do
    let shared_ports_a = findShared a b
        shared_ports_b = findShared b a
        extra_ports_a = foldl' (flip Map.delete) a shared_ports_a
        extra_ports_b = foldl' (flip Map.delete) b shared_ports_b

    Just $ Layout (0,0) (Map.union extra_ports_a extra_ports_b)

findShared :: HashMap Port (PortKind, [Tile]) -> HashMap Port (PortKind, [Tile]) -> [Port]
findShared mapA mapB = do
    catMaybes $ map (\p@(t,d) -> pure p <$> Map.lookup (t, d_invert d) mapB) $ Map.keys mapA
  where
    d_invert In = Out
    d_invert Out = In

testSplitGraph :: Tree Text
testSplitGraph = do
    Node "merge"
        [ Node "merge" 
            [ Node "craft1" []
            , Node "merge"
                [ Node "craft2" []
                , Node "craft4" [] ]]
        , Node "merge"
            [ Node "merge"
                [ Node "craft3" []
                , Node "craft5" []]
            , Node "Balance" []]]

testLayout = do
    let (g, l, m) = testGraph
        tree = testSplitGraph

        wire = Layout 
            (3,3) 
            (Map.fromList 
                [ (("copper_plate", In),
                    (Grabber, 
                        [ Tile (0,0)
                        , Tile (0,1)
                        , Tile (0,2)
                        , Tile (1,0)
                        , Tile (0,2)
                        , Tile (2,0)
                        , Tile (2,1)
                        , Tile (2,2)
                        ]))
                , (("copper_wire", Out),
                    (Grabber, 
                        [ Tile (0,0)
                        , Tile (0,1)
                        , Tile (0,2)
                        , Tile (1,0)
                        , Tile (0,2)
                        , Tile (2,0)
                        , Tile (2,1)
                        , Tile (2,2)
                        ]))
                ])

        circuit = Layout 
            (3,3) 
            (Map.fromList 
                [ (("copper_wire", In),
                    (Grabber, 
                        [ Tile (0,0)
                        , Tile (0,1)
                        , Tile (0,2)
                        , Tile (1,0)
                        , Tile (0,2)
                        , Tile (2,0)
                        , Tile (2,1)
                        , Tile (2,2)
                        ]))
                , (("iron_plate", In), 
                    (Grabber, 
                        [ Tile (0,0)
                        , Tile (0,1)
                        , Tile (0,2)
                        , Tile (1,0)
                        , Tile (0,2)
                        , Tile (2,0)
                        , Tile (2,1)
                        , Tile (2,2)
                        ]))
                , (("green_circuit", Out), 
                    (Grabber, 
                        [ Tile (0,0)
                        , Tile (0,1)
                        , Tile (0,2)
                        , Tile (1,0)
                        , Tile (0,2)
                        , Tile (2,0)
                        , Tile (2,1)
                        , Tile (2,2)
                        ]))
                ])

    (hPutStrLn stdout . show) $ mergeLayouts wire circuit
