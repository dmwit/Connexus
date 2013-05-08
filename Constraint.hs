-- boilerplate {{{1
module Constraint where

import Direction
import Grid ()
import Misc

import Control.Applicative
import Control.Monad
import Data.Array.IO
import Data.Function
import Data.IORef
import Data.Monoid
import Data.Set (Set)
import Data.Universe
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
type Rule = Direction -> Constraint -> Constraint -> (Any, Constraint)

cohere False cOld cNew = (Any False, cOld)
cohere True  cOld cNew = (Any True , cNew)

avoidRule   dir cme cdir = cohere (cdir `can'tPoint` aboutFace dir && cme `mightPoint`    dir) cme (avoid dir cme)
connectRule dir cme cdir = cohere (cdir `mustPoint`  aboutFace dir && cme `mightNotPoint` dir) cme (point dir cme)

allRules = [avoidRule, connectRule]

-- solver {{{1
data Solver = Solver {
	constraints :: IOArray Point Constraint,
	dirty       :: IORef (Set Point)
	}

getNeighbors :: Solver -> Point -> IO [(Point, (Direction, Constraint))]
getNeighbors (Solver { constraints = cs }) pos = do
	b <- getBounds cs
	sequence [ (\c -> (pos', (dir, c))) <$> readArray cs pos'
	         | dir <- universe
	         , let pos' = Direction.step dir pos
	         , inRange b pos'
	         ]

-- TODO: use Writer
runRule :: Rule -> Constraint -> [(Direction, Constraint)] -> (Any, Constraint)
runRule rule cme = foldr (\(dir, cdir) (v, current) -> let (v', next) = rule dir current cdir in (v `mappend` v', next)) (mempty, cme)

runRules :: Constraint -> [(Direction, Constraint)] -> (Any, Constraint)
runRules cme neighbors = foldr (\rule (v, current) -> let (v', next) = runRule rule current neighbors in (v `mappend` v', next)) (mempty, cme) allRules

runRulesIO :: Solver -> Point -> IO ()
runRulesIO solver pos = do
	neighbors <- getNeighbors solver pos
	cme       <- readArray (constraints solver) pos
	let (Any changed, after) = runRules cme (map snd neighbors)
	when changed $ do
		writeArray (constraints solver) pos after
		modifyIORef (dirty solver) (Set.union . Set.fromList $ map fst neighbors)

step :: Solver -> IO ()
step solver@(Solver { constraints = cs, dirty = dref }) = do
	d <- readIORef dref
	case Set.minView d of
		Nothing       -> return ()
		Just (pos, d) -> writeIORef dref d >> runRulesIO solver pos

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
	mapM_ (\ls -> mapM_ (\d -> moveTo x' y' >> lineTo (x' + dx d / 2) (y' + dy d / 2)) ls >> stroke) listc
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
