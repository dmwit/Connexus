-- boilerplate {{{1
module Constraint where

import Direction
import Grid ()
import Misc

import Control.Monad
import Data.Array.IO
import Data.Function
import Data.IORef
import Data.Set (Set)
import Graphics.Rendering.Cairo

import qualified Data.Set as Set

-- constraint properties {{{1
type Constraint = Set (Set Direction)

setAll        :: Ord a => (a -> Bool) -> (Set a -> Bool)
setAny        :: Ord a => (a -> Bool) -> (Set a -> Bool)
notMember     :: Ord a => a -> Set a -> Bool
determined    :: Constraint -> Bool
contradiction :: Constraint -> Bool
mustPoint     :: Constraint -> Direction -> Bool
can'tPoint    :: Constraint -> Direction -> Bool
mightPoint    :: Constraint -> Direction -> Bool
mightNotPoint :: Constraint -> Direction -> Bool
point         :: Direction -> Constraint -> Constraint
avoid         :: Direction -> Constraint -> Constraint

setAll p = Set.null . Set.filter (not . p)
setAny p = not . setAll (not . p)
notMember  a  = not . Set.member a
determined c  = Set.size c == 1
contradiction = Set.null
mustPoint     = flip (setAll . Set.member)
can'tPoint    = flip (setAll . notMember )
mightPoint    = flip (setAny . Set.member)
mightNotPoint = flip (setAny . notMember )
point = Set.filter . Set.member
avoid = Set.filter . notMember

-- rules {{{1
-- TODO: this type is awful:
-- 1. why not return Maybe Constraint?
-- 2. actually, why not just return Constraint, since we throw away the result
--    of checking whether we should apply first anyway
data Rule = Rule {
	offsets   :: [Point],
	constrain :: [Constraint] -> Constraint -> Maybe (Constraint -> Constraint)
	}

avoidRule dir = Rule {
	offsets   = [(dx dir, dy dir)],
	constrain = \[cdir] cme ->
		guard (cdir `can'tPoint` aboutFace dir && cme `mightPoint` dir) >> return (avoid dir)
	}

connectRule dir = Rule {
	offsets     = [(dx dir, dy dir)],
	constrain   = \[cdir] cme ->
		guard (cdir `mustPoint` aboutFace dir && cme `mightNotPoint` dir) >> return (point dir)
	}

allRules =
	map avoidRule   [minBound..maxBound] ++
	map connectRule [minBound..maxBound]

-- solver {{{1
data Solver = Solver {
	constraints :: IOArray Point Constraint,
	dirty       :: IORef (Set Point)
	}

getOffsets :: Point -> [Point] -> Solver -> IO (Maybe [Point])
getOffsets (x, y) offsets (Solver { constraints = cs }) = do
	b <- getBounds cs
	return $ ensure (all (inRange b)) (map (\(dx, dy) -> (x + dx, y + dy)) offsets)

-- TODO: should be returning points with the *negative* offsets
--       (we're just getting lucky that every rule has a symmetric version with the opposite offset)
-- TODO: there might be something to say for enforcing rotational and reflectional symmetry in the rules, actually
runRule :: Rule -> Constraint -> Point -> Solver -> IO ([Point], Constraint -> Constraint)
runRule rule c pos solver = do
	mNeighbors <- getOffsets pos (offsets rule) solver
	case mNeighbors of
		Nothing -> return ([], id)
		Just pNeighbors -> do
			cNeighbors <- mapM (readArray (constraints solver)) pNeighbors
			case constrain rule cNeighbors c of
				Nothing -> return (pNeighbors, id)
				Just f  -> return (pNeighbors, f)

collapsePoints :: Ord a => [[a]] -> Set a
collapsePoints = Set.fromList . concat

runRules :: Constraint -> Point -> Set Point -> Solver -> IO ()
runRules c pos d solver = do
	(ds, fs) <- liftM unzip $ mapM (\rule -> runRule rule c pos solver) allRules
	let c' = foldr ($) c fs
	writeIORef (dirty solver) $ if c == c'
		then d
		else Set.union d . Set.fromList . (pos:) . concat $ ds
	writeArray (constraints solver) pos c'

step :: Solver -> IO ()
step solver@(Solver { constraints = cs, dirty = dref }) = do
	d <- readIORef dref
	case Set.minView d of
		Nothing       -> return ()
		Just (pos, d) -> readArray cs pos >>= \c -> runRules c pos d solver

-- rendering {{{1
update :: Solver -> Render ()
update (Solver { constraints = cs, dirty = dref }) = do
	renderConstraints cs
	d <- liftIO (readIORef dref)
	renderDirty d

renderConstraints cs = do
	setLineWidth 0.4
	setLineCap LineCapRound
	b <- liftIO (getBounds cs)
	mapM_ (renderConstraint cs) (range b)

renderConstraint cs pos@(x, y) = do
	c <- liftIO (readArray cs pos)
	let
		count = fromIntegral $ Set.size c
		listc = map Set.toList (Set.toList c)
	setSourceRGBA 0 0 0 (1 / count)
	mapM_ (mapM_ (\d -> moveTo x' y' >> lineTo (x' + dx d / 2) (y' + dy d / 2) >> stroke)) listc
	stroke
	where
	x' = fromIntegral x
	y' = fromIntegral y

renderDirty poss = do
	setLineWidth 0
	setSourceRGB 0.8 0 0
	mapM_ circle (Set.toList poss)
	fill
	where
	circle' x y = moveTo (x + 0.1) (y + 0.1) >> arc x y 0.1 0 (2 * pi)
	circle = uncurry (circle' `on` fromIntegral)

-- creating {{{1
solverFromGrid :: IOArray Point (Set Direction) -> IO Solver
solverFromGrid nodeShape = do
	b@((xlo, ylo), (xhi, yhi)) <- getBounds nodeShape
	dref <- newIORef (Set.fromList (range b))
	cs   <- newArray ((xlo-1, ylo-1), (xhi+1, yhi+1)) (Set.singleton Set.empty)
	forM_ (range b) $ \pos -> do
		connections <- readArray nodeShape pos
		writeArray cs pos (Set.fromList [rotation connections | rotation <- [id, clockwise, aboutFace, counterclockwise]])
	return Solver { constraints = cs, dirty = dref }
