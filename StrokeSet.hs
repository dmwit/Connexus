{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module StrokeSet (
	StrokeSet,
	strokeHorizontal, strokeVertical,
	horizontals, verticals, strokes
) where

import Interval

import Data.List   (transpose)
import Data.Default
import Data.Map    (Map)
import Data.Maybe
import Data.Monoid

import qualified Data.Map as Map

data StrokeSet h v = StrokeSet {
	hs :: Map v [[Interval h]],
	vs :: Map h [[Interval v]]
	} deriving (Eq, Ord, Show, Read)
instance Default (StrokeSet h v) where def = StrokeSet def def

zipMonoid   :: Monoid m => [[m]] -> [m]
unStrokeSet :: (a -> a -> b) -> [[Interval a]] -> [[b]]
unStrokeMap :: (a -> b -> b -> c) -> Map a ([[Interval b]]) -> [[c]]

zipMonoid        = map mconcat . transpose
unStrokeSet f ss = [[f b e | Interval (Just b, Just e) <- is] | is <- ss]
unStrokeMap f    = zipMonoid . Map.elems . Map.mapWithKey (\a -> unStrokeSet (\bb be -> f a bb be))

strokeMap     :: (Ord a, Ord b) => a -> b -> b -> Map a [[Interval b]] -> Map a [[Interval b]]
strokeAll     :: Ord a => [Interval a] -> [[Interval a]] -> [[Interval a]]

strokeMap a bb be m = flip (Map.insert a) m . strokeAll [closed bb be] . fromMaybe [] . Map.lookup a $ m
strokeAll []      iss  = iss
strokeAll is [       ] = [is]
strokeAll is (is':iss) = union (is ++ is') : strokeAll (intersections is is') iss

-- optimization idea: we know ais/ais' are pre-sorted, so can get O(m+n)
-- implementation rather than O(mn) by merging rather than producting
intersections :: Ord a => [Interval a] -> [Interval a] -> [Interval a]
intersections ais ais' = filter (not .isPoint) $ union [intersect ai ai' | ai <- ais, ai' <- ais']

strokeHorizontal :: (Ord h, Ord v) => v -> h -> h -> StrokeSet h v -> StrokeSet h v
strokeVertical   :: (Ord h, Ord v) => h -> v -> v -> StrokeSet h v -> StrokeSet h v
horizontals      :: StrokeSet h v -> [[((h, v), (h, v))]]
verticals        :: StrokeSet h v -> [[((h, v), (h, v))]]
strokes          :: StrokeSet a a -> [[((a, a), (a, a))]]

strokeHorizontal v hb he ss = ss { hs = strokeMap v hb he (hs ss) }
strokeVertical   h vb ve ss = ss { vs = strokeMap h vb ve (vs ss) }
horizontals = unStrokeMap (\v hb he -> ((hb, v), (he, v))) . hs
verticals   = unStrokeMap (\h vb ve -> ((h, vb), (h, ve))) . vs
strokes gss = zipMonoid [horizontals gss, verticals gss]
