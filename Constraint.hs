-- boilerplate {{{1
{-# LANGUAGE FlexibleContexts #-}
module Constraint where

import Direction
import Grid ()
import Misc

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Array.IO
import Data.Default
import Data.Function
import Data.Set (Set)
import Data.Graph.Inductive
import Data.IORef
import Data.Universe
import Graphics.Rendering.Cairo

import qualified Data.Set as Set

-- queue {{{1
data Queue a = Queue
	{ front    :: [a]
	, back     :: [a]
	, elements :: Set a
	} deriving (Eq, Ord, Show, Read)
instance Default (Queue a) where def = Queue def def def

push e q
	| Set.member e (elements q) = q
	| otherwise = q { back = e:back q, elements = Set.insert e (elements q) }

pop q@(Queue { front = e:front', elements = es }) = Just (e, q { front = front', elements = Set.delete e es })
pop q@(Queue { front = [], back = [] }) = Nothing
pop q@(Queue { front = [] }) = pop q { front = reverse (back q), back = [] }

fromList = foldr push def
toList q = front q ++ back q
union q q' = foldr push q' (back q ++ reverse (front q))

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
type Rule = Constraint -> (Direction, Constraint) -> Writer Any Constraint

cohere False cOld cNew = writer (cOld, Any False)
cohere True  cOld cNew = writer (cNew, Any True )

avoidRule   cme (dir, cdir) = cohere (cdir `can'tPoint` aboutFace dir && cme `mightPoint`    dir) cme (avoid dir cme)
connectRule cme (dir, cdir) = cohere (cdir `mustPoint`  aboutFace dir && cme `mightNotPoint` dir) cme (point dir cme)

bothRules cme cdir = ((`avoidRule` cdir) >=> (`connectRule` cdir)) cme

-- solver {{{1
data Solver = Solver {
	constraints :: IOArray Point Constraint,
	dirty       :: IORef (Queue Point)
	}

getNeighbors :: Solver -> Point -> IO [(Point, (Direction, Constraint))]
getNeighbors (Solver { constraints = cs }) pos = do
	b <- getBounds cs
	sequence [ (\c -> (pos', (dir, c))) <$> readArray cs pos'
	         | dir <- universe
	         , let pos' = Direction.step dir pos
	         , inRange b pos'
	         ]

runRulesIO :: Solver -> Point -> IO ()
runRulesIO solver pos = do
	neighbors <- getNeighbors solver pos
	cme       <- readArray (constraints solver) pos
	let (after, Any changed) = runWriter . foldM bothRules cme $ map snd neighbors
	when changed $ do
		writeArray (constraints solver) pos after
		modifyIORef (dirty solver) (union . fromList $ map fst neighbors)

step :: Solver -> IO ()
step solver@(Solver { constraints = cs, dirty = dref }) = do
	d <- readIORef dref
	case pop d of
		Nothing       -> return ()
		Just (pos, d) -> writeIORef dref d >> runRulesIO solver pos

-- conversion to a graph {{{2
graphPure :: Graph g => [(Point, Constraint)] -> g Point ()
graphPure assocs = mkGraph labeledPoints edges where
	labeledAssocs = zipWith (\i (p, c) -> (i, p, c)) universe assocs
	labeledPoints = zipWith (\i (p, c) -> (i, p   )) universe assocs
	label p = [i | (i, p') <- labeledPoints, p == p']
	edges   = [ (i, i', ())
	          | (i, p, c) <- labeledAssocs
	          , dir       <- universe
	          , c `mustPoint` dir
	          , i'        <- label (Direction.step dir p)
	          ]

graph :: Graph g => Solver -> IO (g Point ())
graph (Solver { constraints = cs }) = graphPure <$> getAssocs cs

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
	mapM_ circle (toList poss)
	fill
	where
	circle' x y = moveTo (x + 0.1) (y + 0.1) >> arc x y 0.1 0 (2 * pi)
	circle = uncurry (circle' `on` fromIntegral)

-- creating {{{1
-- like range, but spiral inwards
range' ((xlo, ylo), (xhi, yhi))
	| xlo > xhi || ylo > yhi = []
	| otherwise =  [(x, ylo) | x <- [xlo .. xhi]]
	            ++ [(x, yhi) | x <- [xlo .. xhi]]
	            ++ [(xlo, y) | y <- [ylo+1 .. yhi-1]]
	            ++ [(xhi, y) | y <- [ylo+1 .. yhi-1]]
	            ++ range' ((xlo+1,ylo+1),(xhi-1,yhi-1))

solverFromGrid :: IOArray Point (Set Direction) -> IO Solver
solverFromGrid nodeShape = do
	b@((xlo, ylo), (xhi, yhi)) <- getBounds nodeShape
	dref <- newIORef (fromList (reverse (range' b)))
	cs   <- newArray ((xlo-1, ylo-1), (xhi+1, yhi+1)) (Set.singleton Set.empty)
	forM_ (range b) $ \pos -> do
		connections <- readArray nodeShape pos
		writeArray cs pos (Set.fromList [rotation connections | rotation <- [id, clockwise, aboutFace, counterclockwise]])
	return Solver { constraints = cs, dirty = dref }
