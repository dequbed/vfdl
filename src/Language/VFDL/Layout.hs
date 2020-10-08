{-# LANGUAGE DeriveAnyClass #-}
module Language.VFDL.Layout
    ( testLayout
    ) where

import Prelude hiding ((<|>))

import GHC.Float (int2Float)
import System.IO
import System.IO.Unsafe

import Language.VFDL.Compile
import Language.VFDL.TLR

import Data.Matrix
import Data.Tree

import qualified Prelude.List as L
import qualified Prelude.List.Partial as L'

import qualified Data.HashMap.Strict as Map

data Tile = Tile (Int, Int)
    deriving (Eq, Ord, Show, Generic, Hashable)

data TileObj = TileObj
    { tileCenter :: Bool
    , tileEntity :: Entity
    } deriving (Eq, Show)

buildTileObj ent = TileObj False ent

data Direction = In | Out
    deriving (Eq, Ord, Show, Generic, Hashable)
type Port = (Text, Direction)

-- |An 'edge' in-/output can be from any side.
-- TODO: Differentiate lanes. Especially important for underground belts
data Location = LocTop | LocLeft | LocRight | LocBottom
    deriving (Eq, Ord, Show, Generic, Hashable)

-- |Define the kind of in-/output this port is
-- Belt means an "edge" in-/output. A belt placed with any of its three input sides against this
-- port will take items.
-- Grabber means "top" in-/output. A belt placed next to it will not receive items but they have to
-- be extracted using a Grabber.
-- TODO: Make this more generic so loaders are better represented.
data PortKind = Belt Location Tile | Grabber Tile
    deriving (Eq, Ord, Show, Generic, Hashable)

-- |Layout of the factory being described
-- The important part of layouts is how you combine them — i.e. how to layouts are merged into one
-- larger one.
data Layout = Layout 
    { layout_dimensions :: (Int, Int)
    -- ^ Bounding box of the layout.
    -- TODO: Make this not a box but any shape
    , layout_io_blocks :: HashMap Port ([PortKind]) 
    -- ^ IO of a (sub)factory.
    -- It is crucial to ensure that IO ports that are not satisfied in a merge (satisfied means an
    -- output of one layout is an input of the other one) must be kept reachable for later merges.
    , layout_matrix :: Matrix (Maybe TileObj)
    -- ^ The actual layout of the factory. 
    -- TODO: Having this as a full matrix is somewhat memory inefficient, is there a more sensible
    -- way?
    } deriving (Eq, Show)

-- |Merge two layouts if a solution could be found
mergeLayouts :: Layout -> Layout -> Maybe Layout
mergeLayouts la@(Layout dim_a io_a m_a) lb@(Layout dim_b io_b m_b) = do
    let shared_ports_a = findShared io_a io_b
        shared_ports_b = findShared io_b io_a
        extra_ports_a = foldl' (flip Map.delete) io_a shared_ports_a
        extra_ports_b = foldl' (flip Map.delete) io_b shared_ports_b

    case shared_ports_a of
      [] -> do
          -- When there are no shared ports we just glue them together
          undefined
      [port] -> do
          -- When we only share one port, the connection is somewhat trivial: Just glue them together
          trivialConnect la lb port
      xs -> do
          fail "Not implemented yet"
          Nothing


-- The trivial connect that is very much not trivial
trivialConnect :: Layout -> Layout -> Port -> Maybe Layout
trivialConnect (Layout dim_a io_a m_a) (Layout dim_b io_b m_b) p = do
    -- Yay for monadic application - this will return early if a `Nothing` is encountered
    source_ports <- Map.lookup p io_a
    dest_ports <- Map.lookup (portOther p) io_b

    let wa = ncols m_a
        ha = nrows m_a
        wb = ncols m_b
        hb = nrows m_b
        sloc = map (portLoc wa ha) source_ports
        dloc = map (portLoc wb hb) dest_ports
        shared_ports_a = findShared io_a io_b
        shared_ports_b = findShared io_b io_a
        extra_ports_a = foldl' (flip Map.delete) io_a shared_ports_a
        extra_ports_b = foldl' (flip Map.delete) io_b shared_ports_b

    -- FIXME: Sanity check that we didn't obstruct any other ports this way!
    if L.any (== LocRight) sloc && L.any (== LocLeft) dloc then do
            -- Get an (rather abitrary) port to connect. Read this spaghetti from right to left: We
            -- first combine the location of the ports back with the ports using zip ([]-lists keep
            -- their ordering trough `map`s!), then we filter on the first tuple element (i.e. the
            -- location), get the first result that fits our needs and then extract the second tuple
            -- element (i.e. the port itself)
        let port_a = snd $ L'.head $ L.filter ((== LocRight) . fst) $ zip sloc source_ports
            port_b = snd $ L'.head $ L.filter ((== LocLeft) . fst) $ zip dloc dest_ports

            h_port_a = height port_a
            h_port_b = height port_b

            outer_height_above = (max h_port_a h_port_b)
            outer_height = outer_height_above + (max (ha - h_port_a) (hb - h_port_b))

            a_pos_y = outer_height_above - h_port_a
            b_pos_y = outer_height_above - h_port_b

            -- a_pos_x is always 0
            -- right matrix needs to leave some space for the grabber and/or belt if any
            b_pos_x = wa + addWidth port_a port_b
            -- Extend the matrixes to have the same amount of rows since that is a hard requirement
            -- of `<|>`
            top_m_a = if a_pos_y > 0 then (matrix a_pos_y wa (pure Nothing)) <-> m_a else m_a
            new_ma = extendTo Nothing outer_height wa top_m_a

            top_m_b = if b_pos_y > 0 then (matrix b_pos_y wb (pure Nothing)) <-> m_b else m_b
            new_mb = extendTo Nothing outer_height wb top_m_b

            -- The final matrix of the new layout. It either consists of the two matrices with a
            -- connecting matrix in bettween (e.g. containing the grabber) or just the two matrices
            -- glued together directly
            final_matrix = if 0 /= addWidth port_a port_b 
                then new_ma <|> conn_matrix outer_height outer_height_above port_a port_b <|> new_mb
                else new_ma <|> new_mb

        -- TODO unionWith here but that requires the rest to understand how to treat the list of ports!
        Just $ Layout (nrows final_matrix, ncols final_matrix) (Map.union extra_ports_a extra_ports_b) final_matrix

    else if L.any (== LocBottom) sloc && L.any (== LocTop) dloc then do
        undefined
    else
        undefined
  where
      portOther (a, In) = (a, Out)
      portOther (a, Out) = (a, In)
      height (Belt _ t) = height' t
      height (Grabber t) = height' t
      height' (Tile (_,y)) = y
      addWidth (Belt _ _) (Belt _ _) = 0
      addWidth (Grabber _) (Grabber _) = 3
      addWidth _ _ = 4

conn_matrix :: Int -> Int -> PortKind -> PortKind -> Matrix (Maybe TileObj)
conn_matrix n h (Grabber _) (Grabber _) =
    let grabber = TileObj True $ Entity 0 "inserter" (Position 0 0) (Just 1) Nothing Nothing Nothing Nothing
    in matrix n 1 (\case (x,y) | x == h && y == 1 -> Just grabber
                         _ -> Nothing)


routeBotToRight = undefined
routeTopToLeft = undefined
routeOutside = undefined

connectTopBottom :: Layout -> Layout -> Maybe Layout
connectTopBottom top bot = do
    -- If there are ports on the bottom of the top layout route them away to not obstruct them
    let (Layout top_dim@(top_x, top_y) top_io top_matrix)
            = if hasBotPorts top 
                then routeBotToRight top 
                else top
    -- Same for the bottom with top ports
        (Layout bot_dim@(bot_x, bot_y) bot_io bot_matrix)
            = if hasTopPorts bot 
                then routeTopToLeft bot
                else bot

        new_x = max top_x bot_x
        new_dim = (new_x, top_y + bot_y)
        -- Generate the new matrix by splitting each into rows, padding the rows to all be as long
        -- as the widest matrix and unsplitting a new matrix from that
        new_matrix' = fromLists $ (map (pad new_x) $ toLists top_matrix) ++ (map (pad new_x) $ toLists bot_matrix)
        (new_io, new_matrix) = routeOutside new_matrix' (top_io, top_dim) (bot_io, bot_dim)

    Just $ Layout new_dim new_io new_matrix
  where
    pad len xs = take len $ xs ++ (L.repeat Nothing)
    pad :: Int -> [Maybe b] -> [Maybe b]

hasBotPorts :: Layout -> Bool
hasBotPorts (Layout (_, y) io matrix) = do
    Map.empty /= Map.filter (L.any (onRow y)) io

hasTopPorts :: Layout -> Bool
hasTopPorts (Layout dim io matrix) = do
    Map.empty /= Map.filter (L.any (onRow 1)) io

onRow :: Int -> PortKind -> Bool
onRow n (Belt _ (Tile (_, cy))) = n == cy
onRow n (Grabber (Tile (_, cy))) = n == cy
onCol :: Int -> PortKind -> Bool
onCol n (Belt _ (Tile (cx, _))) = n == cx
onCol n (Grabber (Tile (cx, _))) = n == cx

portLoc :: Int -> Int -> PortKind -> Location
portLoc width height p = 
    if onRow 1 p then LocTop
    else if onCol 1 p then LocLeft
    else if onRow height p then LocBottom
    else if onCol width p then LocRight
    else unsafePerformIO $ do
        hPutStrLn stderr $ show (width, height, p)
        undefined

findShared :: HashMap Port [PortKind] -> HashMap Port [PortKind] -> [Port]
findShared mapA mapB = do
    catMaybes $ map (\p@(t,d) -> pure p <$> Map.lookup (t, d_invert d) mapB) $ Map.keys mapA
  where
    d_invert In = Out
    d_invert Out = In

data Orientation = Horizontal | Vertical
    deriving (Eq, Show)

complement :: Orientation -> Orientation
complement Horizontal = Vertical
complement Vertical = Horizontal

-- |Line segment, mayor coordinate, endpoint A, endpoint B.
type Line = (Int, Maybe Int, Maybe Int)

--hightower :: Orientation -> [Line] -> [Line]

--escapeI :: (Int, Int)

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

testLayout :: IO [Entity]
testLayout = do
    let (g, l, m) = testGraph
        tree = testSplitGraph

        asm a = fromList 3 3 $ map Just $
            [a ,a ,a
            ,a ,a{tileCenter=True} ,a
            ,a ,a ,a]

        asm_wire = asm $ buildTileObj $ Entity 0 "assembling-machine-1" (Position 0 0) Nothing (Just "copper-cable") Nothing Nothing Nothing

        wire s = Layout 
            (3,3) 
            (Map.fromList 
                [ (("copper_plate", In),
                    [ Grabber $ Tile (1,1)
                    , Grabber $ Tile (1,2)
                    , Grabber $ Tile (1,3)
                    , Grabber $ Tile (2,1)
                    , Grabber $ Tile (2,3)
                    , Grabber $ Tile (3,1)
                    , Grabber $ Tile (3,2)
                    , Grabber $ Tile (3,3)
                    ])
                , ((s, Out),
                    [ Grabber $ Tile (1,1)
                    , Grabber $ Tile (1,2)
                    , Grabber $ Tile (1,3)
                    , Grabber $ Tile (2,1)
                    , Grabber $ Tile (2,3)
                    , Grabber $ Tile (3,1)
                    , Grabber $ Tile (3,2)
                    , Grabber $ Tile (3,3)
                    ])
                ])
            asm_wire

        asm_circ = asm $ buildTileObj $ Entity 0 "assembling-machine-1" (Position 0 0) Nothing (Just "electronic-circuit") Nothing Nothing Nothing

        circuit = Layout 
            (3,3) 
            (Map.fromList 
                [ (("copper_wire_1", In),
                    [ Grabber $ Tile (1,1)
                    , Grabber $ Tile (1,2)
                    , Grabber $ Tile (1,3)
                    , Grabber $ Tile (2,1)
                    , Grabber $ Tile (2,3)
                    , Grabber $ Tile (3,1)
                    , Grabber $ Tile (3,2)
                    , Grabber $ Tile (3,3)
                    ])
                , (("copper_wire_2", In),
                    [ Grabber $ Tile (1,1)
                    , Grabber $ Tile (1,2)
                    , Grabber $ Tile (1,3)
                    , Grabber $ Tile (2,1)
                    , Grabber $ Tile (2,3)
                    , Grabber $ Tile (3,1)
                    , Grabber $ Tile (3,2)
                    , Grabber $ Tile (3,3)
                    ])
                , (("iron_plate", In), 
                    [ Grabber $ Tile (1,1)
                    , Grabber $ Tile (1,2)
                    , Grabber $ Tile (1,3)
                    , Grabber $ Tile (2,1)
                    , Grabber $ Tile (2,3)
                    , Grabber $ Tile (3,1)
                    , Grabber $ Tile (3,2)
                    , Grabber $ Tile (3,3)
                    ])
                , (("green_circuit", Out), 
                    [ Grabber $ Tile (1,1)
                    , Grabber $ Tile (1,2)
                    , Grabber $ Tile (1,3)
                    , Grabber $ Tile (2,1)
                    , Grabber $ Tile (2,3)
                    , Grabber $ Tile (3,1)
                    , Grabber $ Tile (3,2)
                    , Grabber $ Tile (3,3)
                    ])
                ])
            asm_circ

    case mergeLayouts (wire "copper_wire_2") =<< mergeLayouts (wire "copper_wire_1") circuit of
        Just layout -> do
            let entity_list = getEntities $ layout_matrix layout
            return $ entity_list
        Nothing -> do
            fail "Failed to converge layout"

getEntities :: Matrix (Maybe TileObj) -> [Entity]
getEntities = map setEntNumber . zip [1..] . foldl' getEntities' [] . mapPos setEntPos

setEntPos :: ((Int, Int) -> Maybe TileObj -> Maybe TileObj)
setEntPos (x,y) (Just (TileObj True e)) = Just $ TileObj True $ e { entityPosition = (Position (0.5 + int2Float x) (0.5 + int2Float y)) }
setEntPos _ _ = Nothing

getEntities' :: [Entity] -> Maybe TileObj -> [Entity]
getEntities' xs (Just (TileObj True e)) = e:xs
getEntities' xs _ = xs

setEntNumber :: (Int, Entity) -> Entity
setEntNumber (x,e) = e { entityNumber = x }
