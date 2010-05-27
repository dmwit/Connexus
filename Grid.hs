-- boilerplate {{{
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Grid where

import Direction
import Graph
import Interval
import Misc
import StrokeSet

import Control.Monad.Random
import Control.Monad.State
import Data.Array
import Data.Array.IO
import Data.Default
import Data.Function
import Data.IntMap (IntMap)
import Data.List
import Data.Map (Map)
import Graphics.Rendering.Cairo hiding (rotate)

import qualified Data.IntMap as IntMap
import qualified Data.Map    as Map
-- }}}
-- types {{{
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
	nodeBackend :: Array (Point, Direction) NodeId,
	nodeShape   :: IOArray Point [Connection],
	edgeShape   :: IntMap (EdgeType, (Point, Direction))
	}
-- }}}
defaultDelay = 0.05
-- creating {{{
blankGridComponents w h = do
	backend <- newArray (((0, 0), minBound), ((w, h), maxBound)) maxBound
	nodeIds <- liftM (listArray ((0, 0), (w,h))) (replicateM ((w+1) * (h+1)) addNode)
	forM_ (range (0, w)) $ \x -> addNode >>= writeArray backend ((x, 0), North)
	forM_ (range (0, h)) $ \y -> addNode >>= writeArray backend ((0, y), West )
	forM_ (range ((0, 0), (w, h))) $ \(x, y) -> do
		east  <- addNode
		south <- addNode
		writeArray backend ((x, y), East ) east
		writeArray backend ((x, y), South) south
		when (x < w) (writeArray backend ((x+1, y), West ) east )
		when (y < h) (writeArray backend ((x, y+1), North) south)
	frozen <- unsafeFreeze (backend :: IOArray (Point, Direction) NodeId)
	shape  <- newArray ((0, 0), (w, h)) []
	return (nodeIds, frozen, shape)

connect shape p d i o = do
	conns <- readArray shape p
	writeArray shape p (Connection d i o : conns)

-- assumptions: non-empty map, each key is in the first quadrant, and each value is actually a non-empty set
unsafeStaticGrid :: [(Point, [Direction])] -> IO Grid
unsafeStaticGrid es = flip evalStateT def $ do
	(nodeIds, backend, shape) <- blankGridComponents w h
	esss <- forM es $ \(p, ds) -> do
		let n = nodeIds ! p
		when (p == (0, 0)) $ time >>= signalGraph n . openRight . (+3)
		forM ds $ \d -> do
			let border = backend ! (p, d)
			incoming <- addEdge border n defaultDelay
			outgoing <- addEdge n border defaultDelay
			connect shape p d incoming outgoing
			return [(incoming, (Incoming, (p, d))), (outgoing, (Outgoing, (p, d)))]

	this <- get
	return Grid {
		width  = w,
		height = h,
		graph  = this,
		points = nodeIds,
		nodeBackend = backend,
		nodeShape   = shape,
		edgeShape   = IntMap.fromList . concat . concat $ esss
		}
	where
	w = maximum (map (fst . fst) es)
	h = maximum (map (snd . fst) es)

uniform = fromList . flip zip (repeat 1)
inBound x w = 0 <= x && x < w

randomGrid w h = do
	(nodeIds, backend, shape) <- blankGridComponents (w-1) (h-1)
	origin <- getRandomR ((0, 0), (w-1, h-1))
	es     <- randomGrid' backend nodeIds shape def [origin]
	this   <- get

	return Grid {
		width  = w,
		height = h,
		graph  = this,
		points = nodeIds,
		nodeBackend = backend,
		nodeShape   = shape,
		edgeShape   = es
	}
	where
	randomGrid' backend nodeIds nodeShape = randomGrid'' where
		randomGrid'' edgeShape [] = return edgeShape
		randomGrid'' edgeShape ps = do
			p@(x, y) <- uniform ps
			already  <- liftM (map direction) $ readArray nodeShape p
			possible <- flip filterM [minBound..maxBound] $ \d ->
				if   inBound (x + dx d) w && inBound (y + dy d) h
				then liftM null (readArray nodeShape (x + dx d, y + dy d))
				else return False
			case (already, possible) of
				(_:_:_:_, _ ) -> randomGrid'' edgeShape (delete p ps)
				(   _   , []) -> randomGrid'' edgeShape (delete p ps)
				(   _   , ds) -> do
					d <- uniform ds
					edgeShape <- link edgeShape p d
					edgeShape <- link edgeShape (x + dx d, y + dy d) (aboutFace d)
					randomGrid'' edgeShape ((x + dx d, y + dy d) : ps)

		link edgeShape p d = do
			let np = nodeIds ! p
			let nd = backend ! (p, d)
			i <- addEdge nd np defaultDelay
			o <- addEdge np nd defaultDelay
			connect nodeShape p d i o
			return . IntMap.insert i (Incoming, (p, d))
			       . IntMap.insert o (Outgoing, (p, d))
			       $ edgeShape
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

renderEndpoints now grid poss = do
	setLineWidth 0
	mark 0.3 unlit
	mark 0.9 lit
	where
	litTimes pos = queryNode (points grid ! pos) (graph grid)
	isLit    pos = any (`hasPoint` now) (litTimes pos)
	(lit, unlit) = partition isLit poss
	circle' x y  = moveTo (x + 0.1) (y + 0.1) >> arc x y 0.1 0 (2 * pi)
	circle       = uncurry (circle' `on` fromIntegral)
	mark g poss  = setSourceRGB 0 g 0 >> mapM_ circle poss >> fill

update grid = do
	now       <- time
	endpoints <- filterM (liftM (null . drop 1) . readArray (nodeShape grid))
	                     (range ((0, 0), (width grid - 1, height grid - 1)))
	return $ do
		setLineWidth 0.4
		setLineCap LineCapRound
		renderStrokeSet renderInitGrid   renderEdgeGrid   (gridStrokes   now grid)
		renderStrokeSet renderInitSignal renderEdgeSignal (signalStrokes now grid)
		renderEndpoints now grid endpoints

stable = Graph.stable . graph
-- }}}
-- modifying {{{
onGraph :: MonadState Grid m => State (Graph NodeId EdgeId Double) a -> m a
onGraph m = do
	grid <- get
	let (a, graph') = runState m (graph grid)
	put grid { graph = graph' }
	return a

signal :: MonadState Grid m => Point -> Double -> m ()
signal p t = do
	nodeIds <- gets points
	onGraph (signalGraph (nodeIds ! p) (openRight t))

rotatePointRandomly pos = do
	shape    <- gets nodeShape
	ds       <- liftM (map direction) (readArray shape pos)
	rotation <- uniform $ case map aboutFace ds \\ ds of
		[] -> [id, clockwise, counterclockwise]
		_  -> [id, clockwise, counterclockwise, aboutFace]
	rotate rotation pos

rotateGridRandomly = do
	w <- gets width
	h <- gets height
	mapM_ rotatePointRandomly (range ((0, 0), (w-1, h-1)))

-- TODO: break up from being a monolithic function
rotate rotation pos = do
	backend <- gets nodeBackend
	nodes   <- gets nodeShape
	bounds  <- getBounds nodes
	when (inRange bounds pos) $ do
		now           <- time
		connections   <- readArray nodes pos
		nodeCenterId  <- gets ((!pos) . points)
		let [k, f, e] =  sequence [keep, fix, end] connections
		    fds       =  map (rotation . direction) f
		    nodeIds   =  map (\d -> backend ! (pos, d)) fds

		(incoming, outgoing) <- onGraph $ do
			mapM_ (flip deleteEdge now . incoming) e
			mapM_ (flip deleteEdge now . outgoing) e
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
	updateEdgeShape t ids ds = flip (foldr (\(n, d) -> IntMap.insert n (t, (pos, d)))) (zip ids ds)
-- }}}
