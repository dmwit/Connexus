-- boilerplate {{{1
{-# LANGUAGE NoMonomorphismRestriction #-}
module Grid2 where

import Direction
import Graph2
import Misc

import Control.Monad
import Control.Monad.Instances
import Control.Monad.Random
import Control.Monad.Trans
import Data.Array.IO
import Data.Default
import Data.List
import Data.Ord

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
	graph  :: Graph (Node a) t
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

-- TODO: remove this legacy name after completing the merge with all the "2"
-- versions of files
unsafeStaticGrid = staticGrid

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
-- TODO
-- modification {{{1
rotate rotation pos grid = do
	bounds <- getBounds (pieces grid)
	if inRange bounds pos
		then unsafeRotate rotation pos grid
		else return grid

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

signal pos grid = do
	now <- time
	return grid { graph = addSignal (lattice pos) now (graph grid) }
