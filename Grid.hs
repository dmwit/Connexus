-- boilerplate {{{
{-# LANGUAGE FlexibleContexts #-}
module Grid where

import Direction
import Graph
import Interval
import Misc
import StrokeSet

import Control.Monad.State
import Data.Array
import Data.Array.IO
import Data.Default
import Data.Function
import Data.IntMap (IntMap)
import Data.List
import Data.Map (Map)
import Graphics.Rendering.Cairo

import qualified Data.IntMap as IntMap
import qualified Data.Map    as Map
-- }}}
-- types {{{
type Point      = (Int, Int)
type NodeId     = Int
type EdgeId     = Int
data EdgeType   = Incoming | Outgoing deriving (Eq, Ord, Show, Read)
data Connection = Connection {
	direction :: Direction,
	incoming, outgoing :: EdgeId
	} deriving (Eq, Ord, Show, Read)

data Grid = Grid {
	width  :: Int,
	height :: Int,
	graph  :: Graph NodeId EdgeId Double,
	points :: Array Point NodeId,
	nodeBackend :: IOArray (Point, Direction) NodeId,
	nodeShape   :: IOArray Point [Connection],
	edgeShape   :: IntMap (EdgeType, (Point, Direction))
	}
-- }}}
defaultDelay = 1
-- creating {{{
-- assumptions: non-empty map, each key is in the first quadrant, and each value is actually a non-empty set
unsafeStaticGrid :: [(Point, [Direction])] -> IO Grid
unsafeStaticGrid es = flip evalStateT def $ do
	backend <- newArray (((0, 0), minBound), ((w-1, h-1), maxBound)) maxBound
	nodeIds <- newArray ((0, 0), (w-1, h-1)) maxBound
	forM_ (range (0, w-1)) $ \x -> addNode >>= writeArray backend ((x, 0), North)
	forM_ (range (0, h-1)) $ \y -> addNode >>= writeArray backend ((0, y), West )
	forM_ (range ((0, 0), (w-1, h-1))) $ \(x, y) -> do
		east  <- addNode
		south <- addNode
		writeArray backend ((x, y), East ) east
		writeArray backend ((x, y), South) south
		when (x+1 < w) (writeArray backend ((x+1, y), West ) east )
		when (y+1 < h) (writeArray backend ((x, y+1), North) south)

	shape <- newArray ((0, 0), (w-1, h-1)) []
	esss  <- forM es $ \(p, ds) -> do
		n  <- addNode
		writeArray nodeIds p n
		when (p == (0, 0)) $ time >>= signalGraph n . openRight . (+3)
		forM ds $ \d -> do
			border   <- readArray backend (p, d)
			incoming <- addEdge border n defaultDelay
			outgoing <- addEdge n border defaultDelay
			conns    <- readArray shape p
			writeArray shape p (Connection d incoming outgoing : conns)
			return [(incoming, (Incoming, (p, d))), (outgoing, (Outgoing, (p, d)))]

	ps   <- unsafeFreeze (nodeIds :: IOArray Point NodeId)
	this <- get
	return Grid {
		width  = w,
		height = h,
		graph  = this,
		points = ps,
		nodeBackend = backend,
		nodeShape   = shape,
		edgeShape   = IntMap.fromList . concat . concat $ esss
		}
	where
	w = 1 + maximum (map (fst . fst) es)
	h = 1 + maximum (map (snd . fst) es)
-- }}}
-- rendering {{{
type DStrokeSet = StrokeSet Double Double
gridStrokes, signalStrokes :: Double -> Grid -> DStrokeSet

gridStrokes = strokeAll (\_ _ p d -> strokeDirection p d 0 1)
signalStrokes now grid = strokeAll stroke now grid where
	reverse Incoming = (1 -.)
	reverse Outgoing = id
	strokeInterval p d (Interval (Just b, Just e)) = strokeDirection p d b e

	stroke eid edgeType point direction
		= flip (foldr (strokeInterval point direction . reverse edgeType))
		. queryEdge eid now
		. graph
		$ grid

-- optimization idea: only iterate over the edgeShape, rather than all edges in
-- the graph (since we only draw edges in the edgeShape anyway)
strokeAll :: (EdgeId -> EdgeType -> Point -> Direction -> DStrokeSet -> DStrokeSet) ->
             Double -> Grid -> DStrokeSet
strokeAll f now grid = foldr stroke def . Map.assocs . edges . graph $ grid where
	stroke (i, e) = case (lifetime e `hasPoint` now, IntMap.lookup i $ edgeShape grid) of
		(True, Just (t, (p, d))) -> f i t p d
		_ -> id

strokeDirection :: Point -> Direction -> Double -> Double -> DStrokeSet -> DStrokeSet
strokeDirection (x', y') d b' e' = case d of
	North -> strokeVertical   x (y - e) (y - b)
	East  -> strokeHorizontal y (x + b) (x + e)
	South -> strokeVertical   x (y + b) (y + e)
	West  -> strokeHorizontal y (x - e) (x - b)
	where
	[x, y] = map fromIntegral [x', y']
	[b, e] = map (/2) [b', e']

renderStrokeSet init edge = sequence_ . zipWith renderGroup [0..] . strokes where
	renderGroup n es = do
		init n
		mapM_ (edge n) es
		stroke

renderInitGrid 1 = setSourceRGB 0 0 0
renderInitGrid _ = setSourceRGB 1 0 0
renderEdgeGrid _ ((xb, yb), (xe, ye)) = moveTo xb yb >> lineTo xe ye
renderInitSignal 0 = setSourceRGB 0 0 0.6
renderInitSignal 1 = setSourceRGB 0 0 1
renderInitSignal _ = setSourceRGB 1 0 0
renderEdgeSignal _ ((xb, yb), (xe, ye)) = moveTo xb yb >> lineTo xe ye -- TODO

update grid = do
	now <- time
	return $ do
		setLineWidth 0.4
		setLineCap LineCapRound
		renderStrokeSet renderInitGrid   renderEdgeGrid   (gridStrokes   now grid)
		renderStrokeSet renderInitSignal renderEdgeSignal (signalStrokes now grid)

stable = Graph.stable . graph
-- }}}
-- modifying {{{
onGraph :: MonadState Grid m => State (Graph NodeId EdgeId Double) a -> m a
onGraph m = do
	grid <- get
	let (a, graph') = runState m (graph grid)
	put grid { graph = graph' }
	return a

-- TODO: break up from being a monolithic function
rotate :: (MonadIO m, MonadState Grid m, MArray IOArray [Connection] m, MArray IOArray NodeId m) =>
          (Direction -> Direction) -> Point -> m ()
rotate rotation pos = do
	nodes  <- gets nodeShape
	bounds <- getBounds nodes
	when (inRange bounds pos) $ do
		now           <- time
		connections   <- readArray nodes pos
		let [k, f, e] =  sequence [keep, fix, end] connections
		    fds       =  map (rotation . direction) f
		nodeCenterId  <- gets ((!pos) . points)
		nodeIds       <- mapM nodeIdFor fds

		(incoming, outgoing) <- onGraph $ do
			mapM_ (flip endEdge now . incoming) e
			mapM_ (flip endEdge now . outgoing) e
			incoming <- mapM (\nodeId -> startEdge nodeId nodeCenterId defaultDelay now) nodeIds
			outgoing <- mapM (\nodeId -> startEdge nodeCenterId nodeId defaultDelay now) nodeIds
			return (incoming, outgoing)
		
		oldEdgeShape <- gets edgeShape
		modify (\grid -> grid { edgeShape
			= updateEdgeShape Incoming incoming fds
			. updateEdgeShape Outgoing outgoing fds
			$ oldEdgeShape
			})

		writeArray nodes pos (k ++ zipWith3 Connection fds incoming outgoing)
	where
	isRotationOf c c' = direction c == rotation (direction c')
	keep = (\cs c ->      any (c `isRotationOf`) cs ) >>= filter
	fix  = (\cs c -> not (any (`isRotationOf` c) cs)) >>= filter
	end  = (\cs c -> not (any (c `isRotationOf`) cs)) >>= filter
	nodeIdFor d = gets nodeBackend >>= \b -> readArray b (pos, d)
	updateEdgeShape t ids ds = flip (foldr (\(n, d) -> IntMap.insert n (t, (pos, d)))) (zip ids ds)
-- }}}
