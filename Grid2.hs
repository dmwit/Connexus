{-# LANGUAGE NoMonomorphismRestriction #-}
module Grid2 where

import Direction
import Graph2
import Misc

import Control.Monad
import Control.Monad.Instances
import Control.Monad.Trans
import Data.Array.IO
import Data.Default
import Data.List
import Data.Ord

data Node a
	= Lattice { x, y :: a }
	| RightOf { x, y :: a }
	| Above   { x, y :: a }
	deriving (Eq, Ord, Show, Read)

reCons c  = liftM2 c x y
reLattice = reCons Lattice
reRightOf = reCons RightOf
reAbove   = reCons Above

neighbor North (x, y) = Above    x      y
neighbor East  (x, y) = RightOf  x      y
neighbor South (x, y) = Above    x     (y + 1)
neighbor West  (x, y) = RightOf (x - 1) y

data Piece = Piece { north, east, south, west :: Bool }
	deriving (Eq, Ord, Show, Read)
instance Default Piece where def = Piece False False False False

piece   :: (Direction -> Bool) -> Piece
unPiece :: Piece -> (Direction -> Bool)
piece   f       = Piece (f North) (f East) (f South) (f West)
unPiece p North = north p
unPiece p East  = east  p
unPiece p South = south p
unPiece p West  = west  p

clockwise        = liftM4 Piece west north east south
counterclockwise = liftM4 Piece east south west north
aboutFace        = liftM4 Piece south west north east

set    d p = piece (\d' ->  d == d'  || unPiece p d')
unset  d p = piece (\d' ->  d /= d'  && unPiece p d')
toggle d p = piece (\d' -> (d == d') /= unPiece p d')

data Grid a t = Grid {
	pieces :: IOArray (a,a) Piece,
	graph  :: Graph (Node a) t
	}

defaultDelay = 0.05

grid :: (Ix a, Num a, Fractional t, Ord t, MonadIO m) =>
	IOArray (a, a) Piece -> m (Grid a t)
grid array = liftIO $ do
	now  <- time
	xyps <- getAssocs array
	return (Grid array (newGraph now xyps))
	where
	newGraph now xyps = foldr ($) def (xyps >>= inits now)
	inits now (pos, piece) =
		map ($defaultDelay) (onNeighbors (const True)    initializeEdge pos) ++
		map ($now)          (onNeighbors (unPiece piece) addEdge        pos)
	onNeighbors pred f pos@(x, y) = let here = Lattice x y in do
		d <- [minBound .. maxBound]
		guard (pred d)
		[f here (neighbor d pos), f (neighbor d pos) here]

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
