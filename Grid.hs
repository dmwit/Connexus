-- boilerplate {{{1
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.Random
import Control.Monad.Trans
import Data.Array.IO
import Data.Default
import Data.IORef
import Data.List hiding (intersect, union)
import Data.Maybe
import Data.Set (Set)
import Graphics.Rendering.Cairo hiding (rotate, x, y)

import qualified Data.Set as S

-- Node type {{{1
data Node a
	= Lattice { x, y :: a }
	| RightOf { x, y :: a }
	| Above   { x, y :: a }
	deriving (Eq, Ord, Show, Read)

pprint' c1 c2 n = '(' : pprint (x n) ++ c1 : ", " ++ pprint (y n) ++ c2 : ")"
instance PPrint a => PPrint (Node a) where
	pprint n@(Lattice {}) = pprint' ' ' ' ' n
	pprint n@(RightOf {}) = pprint' '+' ' ' n
	pprint n@(Above   {}) = pprint' ' ' '-' n

-- utility functions {{{1
neighbor North (x, y) = Above    x      y
neighbor East  (x, y) = RightOf  x      y
neighbor South (x, y) = Above    x     (y + 1)
neighbor West  (x, y) = RightOf (x - 1) y
lattice        (x, y) = Lattice  x      y

both f s t = f s t . f t s

instance (Ord a, Oriented a) => Oriented (Set a) where
	clockwise        = S.map clockwise
	counterclockwise = S.map counterclockwise
	aboutFace        = S.map aboutFace

-- Grid type {{{1
data Grid a t = Grid {
	pieces :: IOArray (a,a) (Set Direction),
	graph  :: IORef (Graph (Node a) t)
	}
-- }}}
defaultDelay = 0.05
-- creation {{{1
grid :: (Ix a, Num a, Fractional t, Ord t, MonadIO m) =>
	IOArray (a, a) (Set Direction) -> m (Grid a t)
grid array = liftIO $ do
	now  <- time
	xyps <- getAssocs array
	gref <- newIORef (newGraph now xyps)
	return (Grid array gref)
	where
	newGraph now xyps = foldr ($) def (xyps >>= inits now)
	inits now (pos, piece) =
		onNeighbors (addEdge now)                 pos piece ++
		onNeighbors (initializeEdge defaultDelay) pos (S.fromAscList [minBound .. maxBound])
	onNeighbors f pos = map (\d -> both f (lattice pos) (neighbor d pos)) . S.toList

staticGrid :: [(Point, [Direction])] -> IO (Grid Int Double)
staticGrid [] = newArray ((0,0),(0,0)) def >>= grid
staticGrid pds = do
	array <- newArray ((minX, minY), (maxX, maxY)) def
	forM_ pds $ \(p, ds) -> writeArray array p (S.fromList ds)
	grid array
	where
	[minX, minY, maxX, maxY] = [f (map (g . fst) pds) | f <- [minimum, maximum], g <- [fst, snd]]

randomGrid w h = do
	array  <- newArray bounds def
	origin <- getRandomR bounds
	fill array 0 [origin]
	grid array
	where
	bounds = ((0,0), (w-1,h-1))
	fill array n []   = return ()
	fill array n poss = do -- invariant: n = length poss - 1
		i <- getRandomR (0, n)
		let pos@(x, y) = poss !! i
		already  <- readArray array pos
		possible <- flip filterM [minBound .. maxBound] $ \d ->
			if   inRange bounds (step d pos)
			then liftM S.null (readArray array (step d pos))
			else return False
		case (S.toList already, possible) of
			(_:_:_:_, _ ) -> fill array (n-1) (delete pos poss)
			(   _   , []) -> fill array (n-1) (delete pos poss)
			(   _   , ds) -> do
				d <- uniform ds
				modifyArray array (S.insert               d)         pos
				modifyArray array (S.insert . aboutFace $ d) (step d pos)
				fill array (n+1) (step d pos : poss)

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
	liftIO (readIORef (graph grid)) >>= \g ->
		forPoints_ (pieces grid) $ \pos piece ->
			forM_ (S.toList piece) $ \dir ->
				f (lattice pos) (neighbor dir pos) g

markTerminals now signals pos@(x',y') piece = when (S.size piece == 1) $ do
	if lit then setSourceRGB 0 0.9 0 else setSourceRGB 0 0.3 0
	moveTo (x + 0.1) (y + 0.1)
	arc x y 0.1 0 (2 * pi)
	fill
	where
	(x, y)   = (fromIntegral x', fromIntegral y')
	lit      = signals (lattice pos) `contains` now

update grid = do
	now <- time
	g   <- liftIO (readIORef (graph grid))
	setLineWidth 0.4
	setLineCap LineCapRound
	setSourceRGB  0 0 0
	forLiveEdges_ backgroundPath     grid >> stroke
	setSourceRGBA 0 0 1 0.6
	forLiveEdges_ (singleSignal now) grid >> stroke
	forLiveEdges_ (doubleSignal now) grid >> stroke
	forPoints_ (pieces grid) (markTerminals now (querySignals g))
-- modification {{{1
rotate rotation pos grid = do
	bounds <- getBounds (pieces grid)
	when (inRange bounds pos) (unsafeRotate time rotation pos grid)

-- unsafe because no bounds check on pos
unsafeRotate time rotation pos grid = do
	now <- time
	p   <- readArray (pieces grid) pos
	writeArray (pieces grid) pos (rotation p)
	modifyIORef (graph grid) (rotate' now p)
	where
	correct' True False = subEdge
	correct' False True = addEdge
	correct'   _    _   = \now s t -> id
	correct now d b1 b2 = both (correct' b1 b2 now) (lattice pos) (neighbor d pos)
	corrections now p   = [correct now d (d `S.member` p) (d `S.member` rotation p) | d <- [minBound .. maxBound]]
	rotate'     now p   = foldr (.) id (corrections now p)

rotateGridRandomly grid = do
	now    <- time
	bounds <- getBounds (pieces grid)
	mapM_ (unsafeRotatePointRandomly now grid) (range bounds)

-- unsafe because no bounds check on pos
unsafeRotatePointRandomly now grid pos = do
	rotation <- uniform [id, clockwise, aboutFace, counterclockwise]
	unsafeRotate (return now) rotation pos grid

signal pos grid = time >>= modifyIORef (graph grid) . addSignal (lattice pos)
