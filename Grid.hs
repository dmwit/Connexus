module Grid where

import Direction
import Empty
import Graph
import Interval

import Control.Monad.State
import Control.Monad.Fix
import Data.Array.IO
import Data.Function
import Data.IntMap (IntMap)
import Data.Map (Map)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk (drawableGetSize, renderWithDrawable)
import Graphics.UI.Gtk.Gdk.EventM (eventWindow)

import qualified Data.IntMap as IntMap
import qualified Data.Map    as Map

type Point      = (Int, Int)
type NodeId     = Int
type EdgeId     = Int
data EdgeType   = Incoming Int | Outgoing Point Direction deriving (Eq, Ord, Show, Read)
data Connection = Connection Direction Int Int -- incoming, then outgoing edge id

data Grid = Grid {
	width  :: Int,
	height :: Int,
	graph  :: Graph NodeId EdgeId Rational,
	nodeBackend :: IOArray (Point, Direction) NodeId,
	nodeShape   :: IOArray Point [Connection],
	edgeShape   :: IntMap EdgeType
	}

-- assumptions: non-empty map, each key is in the first quadrant, and each value is actually a non-empty set
unsafeStaticGrid :: [(Point, [Direction])] -> IO Grid
unsafeStaticGrid es = flip evalStateT empty $ do
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
		forM ds $ \d -> do
			border   <- liftIO $ readArray backend (p, d)
			incoming <- addEdge border n 1
			outgoing <- addEdge n border 1
			conns    <- liftIO $ readArray shape p
			liftIO $ writeArray shape p (Connection d incoming outgoing : conns)
			return [(incoming, Incoming outgoing), (outgoing, Outgoing p d)]

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

update grid = eventWindow >>= \dw -> liftIO $ do
	(dww, dwh) <- drawableGetSize dw
	let
		lengthInt  = min (dww `div` (1 + width grid)) (dwh `div` (1 + height grid))
		center a b = fromIntegral $ (a - b * lengthInt) `div` 2
		length     = max 0.01 (fromIntegral lengthInt)
		tlx        = center dww (width  grid)
		tly        = center dwh (height grid)

	renderWithDrawable dw $ do
		translate tlx tly
		scale length length
		setLineWidth 0.4
		setLineCap LineCapRound
		mapM_ renderEdgeId . Map.keys . edges . graph $ grid
		stroke

	return True

	where

	fi i = fromIntegral i + 0.5
	renderEdge eidIn eidOut (x, y) d = moveTo (fi x) (fi y) >> lineTo (fi x + dx d / 2) (fi y + dy d / 2)
	renderEdgeId eid = case IntMap.lookup eid (edgeShape grid) of
		Just (Incoming eid') -> case IntMap.lookup eid' (edgeShape grid) of
			Just (Outgoing p d) -> renderEdge eid eid' p d
			_ -> return ()
		_ -> return ()
