module Grid where

import Direction
import Graph
import Interval
import Misc
import StrokeSet

import Control.Monad.State
import Control.Monad.Fix
import Data.Array.IO
import Data.Default
import Data.Function
import Data.IntMap (IntMap)
import Data.Map (Map)
import Graphics.Rendering.Cairo

import qualified Data.IntMap as IntMap
import qualified Data.Map    as Map

type Point      = (Int, Int)
type NodeId     = Int
type EdgeId     = Int
data EdgeType   = Incoming | Outgoing deriving (Eq, Ord, Show, Read)
data Connection = Connection Direction Int Int -- incoming, then outgoing edge id

data Grid = Grid {
	width  :: Int,
	height :: Int,
	graph  :: Graph NodeId EdgeId Rational,
	nodeBackend :: IOArray (Point, Direction) NodeId,
	nodeShape   :: IOArray Point [Connection],
	edgeShape   :: IntMap (EdgeType, (Point, Direction))
	}

-- assumptions: non-empty map, each key is in the first quadrant, and each value is actually a non-empty set
unsafeStaticGrid :: [(Point, [Direction])] -> IO Grid
unsafeStaticGrid es = flip evalStateT def $ do
	backend <- liftIO $ newArray (((0, 0), minBound), ((w-1, h-1), maxBound)) maxBound
	forM_ (range (0, w-1)) $ \x -> addNode >>= liftIO . writeArray backend ((x, 0), North)
	forM_ (range (0, h-1)) $ \y -> addNode >>= liftIO . writeArray backend ((0, y), West )
	forM_ (range ((0, 0), (w-1, h-1))) $ \(x, y) -> do
		east  <- addNode
		south <- addNode
		liftIO $ do
			writeArray backend ((x, y), East ) east
			writeArray backend ((x, y), South) south
			when (x+1 < w) (writeArray backend ((x+1, y), West ) east )
			when (y+1 < h) (writeArray backend ((x, y+1), North) south)

	shape <- liftIO $ newArray ((0, 0), (w-1, h-1)) []
	esss  <- forM es $ \(p, ds) -> do
		n  <- addNode
		when (p == (0, 0)) $ time >>= signalGraph n . openRight . (+3)
		forM ds $ \d -> do
			border   <- liftIO $ readArray backend (p, d)
			incoming <- addEdge border n 1
			outgoing <- addEdge n border 1
			conns    <- liftIO $ readArray shape p
			liftIO $ writeArray shape p (Connection d incoming outgoing : conns)
			return [(incoming, (Incoming, (p, d))), (outgoing, (Outgoing, (p, d)))]

	this <- get
	return Grid {
		width  = w,
		height = h,
		graph  = this,
		nodeBackend = backend,
		nodeShape   = shape,
		edgeShape   = IntMap.fromList . concat . concat $ esss
		}
	where
	w = 1 + maximum (map (fst . fst) es)
	h = 1 + maximum (map (snd . fst) es)

type RStrokeSet = StrokeSet Rational Rational
gridStrokes, signalStrokes :: Rational -> Grid -> RStrokeSet

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

strokeAll :: (EdgeId -> EdgeType -> Point -> Direction -> RStrokeSet -> RStrokeSet) ->
             Rational -> Grid -> RStrokeSet
strokeAll f now grid = foldr stroke def . Map.assocs . edges . graph $ grid where
	stroke (i, e) = case (lifetime e `hasPoint` now, IntMap.lookup i $ edgeShape grid) of
		(True, Just (t, (p, d))) -> f i t p d
		_ -> id

strokeDirection :: Point -> Direction -> Rational -> Rational -> RStrokeSet -> RStrokeSet
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

fr f = f `on` fromRational
renderInitGrid 1 = setSourceRGB 0 0 0
renderInitGrid _ = setSourceRGB 1 0 0
renderEdgeGrid _ ((xb, yb), (xe, ye)) = fr moveTo xb yb >> fr lineTo xe ye
renderInitSignal 0 = setSourceRGB 0 0 0.6
renderInitSignal 1 = setSourceRGB 0 0 1
renderInitSignal _ = setSourceRGB 1 0 0
renderEdgeSignal _ ((xb, yb), (xe, ye)) = fr moveTo xb yb >> fr lineTo xe ye -- TODO

update grid = do
	now <- time
	return $ do
		setLineWidth 0.4
		setLineCap LineCapRound
		renderStrokeSet renderInitGrid   renderEdgeGrid   (gridStrokes   now grid)
		renderStrokeSet renderInitSignal renderEdgeSignal (signalStrokes now grid)

stable = fmap fromRational . Graph.stable . graph
