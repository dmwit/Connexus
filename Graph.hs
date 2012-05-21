-- boilerplate {{{1
{-# LANGUAGE FlexibleContexts #-}

module Graph where

import Bounds
import Interval (closed, openRight)
import Life
import Misc
import TrieCache (TrieCache)
import qualified TrieCache as T

import Control.Monad
import Data.Default
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Prelude hiding (lookup)
import qualified Data.Map as M

-- Edge and Graph data types {{{1
data Edge time = Edge {
	delay :: time,
	life  :: Life time,
	signalCache :: Life time
	} deriving (Eq, Show, Read)
propogation e = stripe (delay e) (life e)

data Graph nodeId time = Graph {
	edges :: Map nodeId (Map nodeId (Edge time)),
	nodes :: TrieCache nodeId (Life time)
	} deriving (Eq, Show, Read)
instance Default (Graph nodeId time) where def = Graph def def

instance (PPrint nodeId, PPrint time, Ord time) => PPrint (Graph nodeId time) where
	pprint (Graph es ns) = conns ++ cache where
		conns = do
			(i, es' ) <- M.assocs es
			(o, edge) <- M.assocs es'
			if isEmpty (life edge) then "" else concat [
				pprint i,
				" -> ",
				pprint o,
				": ",
				pprint (life edge),
				"/",
				pprint (signalCache edge),
				"\n"
				]
		cache = pprint ns

-- utility functions for working with a nested Map {{{1
findWithDef :: (Ord k, Default a) => k -> Map k a -> a
findWithDef = M.findWithDefault def

insert :: (Ord k1, Ord k2) => k1 -> k2 -> v -> Map k1 (Map k2 v) -> Map k1 (Map k2 v)
lookup :: (Ord k1, Ord k2) => k1 -> k2 -> Map k1 (Map k2 v) -> Maybe v
adjust :: (Ord k1, Ord k2) => (v -> v) -> k1 -> k2 -> Map k1 (Map k2 v) -> Map k1 (Map k2 v)

-- we use "flip (uW (uW (flip const)))" instead of "uW union" because
-- unionWith is more efficient when the large map is its first argument
insert   k1 k2 v = flip (M.unionWith (M.unionWith (flip const))) (M.singleton k1 (M.singleton k2 v))
lookup   k1 k2   = M.lookup k1 >=> M.lookup k2
adjust f k1 k2   = M.adjust (M.adjust f k2) k1

-- node operations {{{1
querySignals :: Ord nodeId => Graph nodeId time -> nodeId -> Life time
querySignals g n = T.query [n] (nodes g)

-- TODO: parallelize this, or rather, incrementalize this, and make a separate
-- thread for doing the incremental computations (to keep the interface snappy)
--
-- basic idea:
--    * instead of doing all the propogation at once, do the update only for
--      this path, and return the list of pending updates that are generated as
--      a result
--    * keep a priority queue of updates; the priority should be path length,
--      with ties broken by the order the *original update* that spawned the
--      current update were put in the queue (so, for example, this could be
--      tracked via the time that the spawning update was put in the queue)
--    * in the GUI thread, just stick an update in the queue
--    * in the worker thread, pop updates off, compute them, and update the
--      graph that the GUI thread is looking at
propogateSignal' update diff path@(~(source:rest)) lifetime graph
	| null path          = graph
	| isEmpty lifetime   = graph
	| source `elem` rest = graph
	| otherwise          = foldr ($) graph { nodes = nodes' } modifications where
	outgoing      = M.assocs (findWithDef source (edges graph))
	modifications = [recache source target . propogateSignal' update diff (target:path) (shift edge) | (target, edge) <- outgoing]
	shift edge    = intersect (propogation edge) (delay edge +. lifetime')
	nodes'        = update path lifetime (nodes graph)
	lifetime'     = diff (T.query path nodes') (T.query path (nodes graph))

unPrimePS f nodeId = f [nodeId] . singleton . openRight

addSignal' :: (Ord nodeId, Ord time, Num time) => [nodeId] -> Life time -> Graph nodeId time -> Graph nodeId time
subSignal' :: (Ord nodeId, Ord time, Num time) => [nodeId] -> Life time -> Graph nodeId time -> Graph nodeId time
addSignal  :: (Ord nodeId, Ord time, Num time) =>  nodeId  ->      time -> Graph nodeId time -> Graph nodeId time
subSignal  :: (Ord nodeId, Ord time, Num time) =>  nodeId  ->      time -> Graph nodeId time -> Graph nodeId time

addSignal' = propogateSignal' T.insertM           diff
subSignal' = propogateSignal' (T.insertWith diff) (flip diff)
addSignal  = unPrimePS addSignal'
subSignal  = unPrimePS subSignal'

-- edge operations {{{1
addEdge'       :: (Ord nodeId, Ord time, Num time) => Life time -> nodeId -> nodeId -> Graph nodeId time -> Graph nodeId time
subEdge'       :: (Ord nodeId, Ord time, Num time) => Life time -> nodeId -> nodeId -> Graph nodeId time -> Graph nodeId time
addEdge        :: (Ord nodeId, Ord time, Num time) =>      time -> nodeId -> nodeId -> Graph nodeId time -> Graph nodeId time
subEdge        :: (Ord nodeId, Ord time, Num time) =>      time -> nodeId -> nodeId -> Graph nodeId time -> Graph nodeId time
initializeEdge :: (Ord nodeId, Ord time, Num time) =>      time -> nodeId -> nodeId -> Graph nodeId time -> Graph nodeId time

-- easily one of the most complicated functions in here, used for adding to or subtracting from an edge's lifetime
-- mod: how to change the signal appearing at the target node in observation of the change to this edge
-- overlap: how to compute which part of the change to the lifetime is actually a change, and which is shared with the old edge
-- combine: how to modify the edge's lifetime once we know the actual change to make
-- newProp: the old edge and the new edge may have different propogation characteristics; newProp tells how to pick out the important changes in this characteristic
-- source, target: which nodes the edge connects
-- edgeLife: the change to the lifetime of the edge
propogateEdge' mod overlap combine newProp edgeLife source target graph = foldr ($) graph' mods where
	oldEdge  = lookup source target (edges graph)
	newLife  = maybe empty (overlap edgeLife . life) oldEdge
	newEdge  = fmap (\e -> e { life = combine newLife (life e) }) oldEdge
	propM    = maybe empty propogation
	propLife = newProp (propM newEdge) (propM oldEdge)
	graph'   = graph { edges = adjust (flip fromMaybe newEdge) source target (edges graph) }
	signals  = T.assocs . T.descend [source] $ nodes graph
	delayM   = maybe 0 delay newEdge -- the 0 should never matter, because anything using it will be thrown away
	mods     = recache source target : [mod (source:target:path) (intersect propLife (delayM +. lifetime)) | (path, lifetime) <- signals]

addEdge' = propogateEdge' addSignal' diff      union       diff
subEdge' = propogateEdge' subSignal' intersect (flip diff) (flip diff)
addEdge  = unPrime addEdge'
subEdge  = unPrime subEdge'
unPrime f' now source target = f' (singleton (openRight now)) source target

initializeEdge delay source target graph = insertInto cleanGraph where
	oldEdge    = lookup source target (edges graph)
	cleanGraph = maybe id (\e -> subEdge' (life e) source target) oldEdge graph
	insertInto = \g -> g { edges = insert source target (Edge delay empty empty) (edges g) }

-- optimization idea: reverse the meaning of 0 and 1 so that we don't have to
-- do a (-.) and hence don't have to do a reverse
queryEdge :: (Ord nodeId, Ord time, Num time, Fractional time) =>
	time -> nodeId -> nodeId -> Graph nodeId time -> Life time
queryEdge now source target = maybe empty scale . lookup source target . edges where
	scale edge = (now -. intersect (live edge) (signalCache edge)) ./ delay edge
	live  edge = intersect (singleton (closed (now - delay edge) now)) (contiguous now (life edge))

-- misc {{{1
instance Ord nodeId => Stable (Graph nodeId) where
	-- even though all the nodes may have stabilized, the entire graph may not have
	-- stabilized yet if there's still a signal traveling on its last leg in a
	-- cycle; to account for this, simply conservatively delay the stable time of
	-- the nodes by the maximal delay of any edge in the graph
	stable g = fmap (maxEdge g +) (maxNode g) where
		maxNode = stable . T.query [] . nodes
		maxEdge = maximum . (0:) . map delay . concatMap M.elems . M.elems . edges

-- optimization idea: instead of recomputing the whole damn lifetime, just incrementally update it
recache :: (Ord nodeId, Ord time) => nodeId -> nodeId -> Graph nodeId time -> Graph nodeId time
recache source target graph = graph { edges = adjust update source target (edges graph) } where
	update e = e { signalCache = signal }
	signal   = T.query [source] . T.deleteSubTrie [source, target] $ nodes graph
	-- @T.deleteSubTrie [source, target]@ makes sure we don't send a signal right back to the node it came from
