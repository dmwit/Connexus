-- boilerplate {{{1
{-# LANGUAGE NoMonomorphismRestriction #-}
module Grid (
	Grid(..), randomGrid, staticGrid, update, rotateGridRandomly, signal, rotate, stable
	) where

import Bounds
import Direction
import Graph
import Interval (unsafeStart, unsafeEnd)
import Life
import Misc

import Control.Monad
import Control.Monad.Instances
import Control.Monad.Random
import Control.Monad.Trans
import Data.Array.IO
import Data.Default
import Data.List hiding (intersect, union)
import Data.Maybe
import Data.Ord
import Graphics.Rendering.Cairo hiding (rotate)

import qualified Data.Map as M

-- Node and Piece types {{{1
data Node a
	= Lattice { x, y :: a }
	| RightOf { x, y :: a }
	| Above   { x, y :: a }
	deriving (Eq, Ord, Show, Read)

data Piece = Piece { north, east, south, west :: Bool }
	deriving (Eq, Ord, Show, Read)
instance Default Piece where def = Piece False False False False

-- utility functions {{{1
neighbor North (x, y) = Above    x      y
neighbor East  (x, y) = RightOf  x      y
neighbor South (x, y) = Above    x     (y + 1)
neighbor West  (x, y) = RightOf (x - 1) y
lattice        (x, y) = Lattice  x      y

both f s t = f s t . f t s

piece   :: (Direction -> Bool) -> Piece
unPiece :: Piece -> (Direction -> Bool)
piece   f       = Piece (f North) (f East) (f South) (f West)
unPiece p North = north p
unPiece p East  = east  p
unPiece p South = south p
unPiece p West  = west  p

instance Oriented Piece where
	clockwise        = liftM4 Piece west north east south
	counterclockwise = liftM4 Piece east south west north
	aboutFace        = liftM4 Piece south west north east

set    d p = piece (\d' ->  d == d'  || unPiece p d')
unset  d p = piece (\d' ->  d /= d'  && unPiece p d')
toggle d p = piece (\d' -> (d == d') /= unPiece p d')

-- Grid type {{{1
data Grid a t = Grid {
	pieces :: IOArray (a,a) Piece,
	graph  :: Graph (Node a) t -- TODO: IORef this
	}
-- }}}
defaultDelay = 0.05
-- creation {{{1
grid :: (Ix a, Num a, Fractional t, Ord t, MonadIO m) =>
	IOArray (a, a) Piece -> m (Grid a t)
grid array = liftIO $ do
	now  <- time
	xyps <- getAssocs array
	return (Grid array (newGraph now xyps))
	where
	newGraph now xyps = foldr ($) def (xyps >>= inits now)
	inits now (pos, piece) =
		onNeighbors (unPiece piece) (addEdge now)                 pos ++
		onNeighbors (const True)    (initializeEdge defaultDelay) pos
	onNeighbors pred f pos = do
		d <- [minBound .. maxBound]
		guard (pred d)
		return (both f (lattice pos) (neighbor d pos))

staticGrid :: [(Point, [Direction])] -> IO (Grid Int Double)
staticGrid [] = newArray ((0,0),(0,0)) def >>= grid
staticGrid pds = do
	array <- newArray ((minX, minY), (maxX, maxY)) def
	forM_ pds $ \(p, ds) -> forM_ ds $ \d -> modifyArray array (toggle d) p
	grid array
	where
	[minX, minY, maxX, maxY] = [f (map (g . fst) pds) | f <- [minimum, maximum], g <- [fst, snd]]

uniform = fromList . flip zip (repeat 1)

randomGrid w h = do
	array  <- newArray bounds def
	origin <- getRandomR bounds
	fill array 0 [origin]
	grid array
	where
	fill array n []   = return ()
	fill array n poss = do -- invariant: n = length poss - 1
		i <- getRandomR (0, n)
		let pos@(x, y) = poss !! i
		already  <- directions array pos
		possible <- flip filterM [minBound .. maxBound] $ \d ->
			if   inRange bounds (step d pos)
			then liftM null (directions array (step d pos))
			else return False
		case (already, possible) of
			(_:_:_:_, _ ) -> fill array (n-1) (delete pos poss)
			(   _   , []) -> fill array (n-1) (delete pos poss)
			(   _   , ds) -> do
				d <- uniform ds
				modifyArray array (set               d)         pos
				modifyArray array (set . aboutFace $ d) (step d pos)
				fill array (n+1) (step d pos : poss)

	bounds = ((0,0), (w-1,h-1))
	directions array pos = do
		p <- readArray array pos
		return . filter (unPiece p) $ [minBound .. maxBound]

-- rendering {{{1
x' l@(RightOf {}) = fromIntegral (x l) + 0.5
x' l              = fromIntegral (x l)
y' l@(Above   {}) = fromIntegral (y l) - 0.5
y' l              = fromIntegral (y l)

signalEitherDirection s t l = forM_ (unLife l) $ \i -> do
	moveTo (x' s + fromMaybe 0 (unsafeStart i) * (x' t - x' s))
	       (y' s + fromMaybe 0 (unsafeStart i) * (y' t - y' s))
	lineTo (x' s + fromMaybe 1 (unsafeEnd   i) * (x' t - x' s))
	       (y' s + fromMaybe 1 (unsafeEnd   i) * (y' t - y' s))

backgroundPath   s t _ = moveTo (x' s) (y' s) >> lineTo (x' t) (y' t)
singleSignal now s t g = signalEitherDirection s t (queryEdge now s t g `union`     (1 -. queryEdge now t s g))
doubleSignal now s t g = signalEitherDirection s t (queryEdge now s t g `intersect` (1 -. queryEdge now t s g))

forPoints_ :: (Ix i, MonadIO m) => IOArray i e -> (i -> e -> m ()) -> m ()
forPoints_ array f = liftIO (getAssocs array) >>= mapM_ (uncurry f)

forLiveEdges_ :: (Ix a, Num a, MonadIO m) =>
	(Node a -> Node a -> Graph (Node a) t -> m ()) -> Grid a t -> m ()
forLiveEdges_ f grid =
	forPoints_ (pieces grid) $ \pos piece ->
		forM_ [minBound .. maxBound] $ \dir ->
			when (unPiece piece dir) (f (lattice pos) (neighbor dir pos) (graph grid))

markTerminals now signals pos@(x',y') piece = when terminal $ do
	if lit then setSourceRGB 0 0.9 0 else setSourceRGB 0 0.3 0
	moveTo (x + 0.1) (y + 0.1)
	arc x y 0.1 0 (2 * pi)
	fill
	where
	terminal = [() | dir <- [north, east, south, west], dir piece] == [()]
	(x, y)   = (fromIntegral x', fromIntegral y')
	lit      = fromMaybe empty (M.lookup (lattice pos) signals) `contains` now

update grid = do
	now <- time
	setLineWidth 0.4
	setLineCap LineCapRound
	setSourceRGB  0 0 0
	forLiveEdges_ backgroundPath     grid >> stroke
	setSourceRGBA 0 0 1 0.6
	forLiveEdges_ (singleSignal now) grid >> stroke
	forLiveEdges_ (doubleSignal now) grid >> stroke
	forPoints_ (pieces grid) (markTerminals now (querySignals (graph grid)))
-- modification {{{1
-- TODO: don't return, just update
rotate rotation pos grid = do
	bounds <- getBounds (pieces grid)
	if inRange bounds pos
		then unsafeRotate rotation pos grid
		else return grid

-- TODO: don't return, just update
unsafeRotate rotation pos grid = do
	now <- time
	p   <- readArray (pieces grid) pos
	writeArray (pieces grid) pos (rotation p)
	return grid { graph = rotate' now p (graph grid) }
	where
	correct' True False = subEdge
	correct' False True = addEdge
	correct'   _    _   = \now s t -> id
	correct now d b1 b2 = both (correct' b1 b2 now) (lattice pos) (neighbor d pos)
	corrections now p   = [correct now d (unPiece p d) (unPiece (rotation p) d) | d <- [minBound .. maxBound]]
	rotate'     now p   = foldr (.) id (corrections now p)

-- TODO: should we call "time" only once here?
rotateGridRandomly grid = getBounds (pieces grid) >>= foldM unsafeRotatePointRandomly grid . range
unsafeRotatePointRandomly grid pos = do
	rotation <- uniform [id, clockwise, aboutFace, counterclockwise]
	unsafeRotate rotation pos grid

-- TODO: don't return, just update
signal pos grid = do
	now <- time
	return grid { graph = addSignal (lattice pos) now (graph grid) }

instance Stable (Grid a) where stable = stable . graph
