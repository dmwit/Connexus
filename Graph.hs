-- boilerplate {{{1
{-# LANGUAGE NoImplicitPrelude #-}

module Graph where

import Bounds
import Interval (closed, openRight)
import Life

import Control.Monad
import Data.Default
import Data.Map (Map)
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
	nodes :: Map [nodeId] (Life time) -- no empty lists as keys
	} deriving (Eq, Show, Read)
instance Default (Graph nodeId time) where def = Graph def def

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
-- optimization idea: cache these
-- to make this efficient, store them in a trie: each entry in the trie is the
-- union of all signals whose path starts with that position in the trie; to
-- recache, find the deepest place in the trie that you're changing and walk it
-- back to the root, recaching as you go
--
-- as a side benefit: this lets you compute the edge signals more quickly as well
querySignals :: (Ord nodeId, Ord time) => Graph nodeId time -> Map nodeId (Life time)
querySignals = M.mapKeysWith union head . M.delete [] {- defensive programming -} . nodes

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
propogateSignal' update path@(~(source:rest)) lifetime graph
	| null path          = graph
	| isEmpty lifetime   = graph
	| source `elem` rest = graph
	| otherwise          = foldr ($) graph' modifications where
	outgoing      = M.assocs (findWithDef source (edges graph))
	modifications = [recache source target . propogateSignal' update (target:path) (shift edge) | (target, edge) <- outgoing]
	graph'        = graph { nodes = update path lifetime (nodes graph) }
	shift edge    = intersect (propogation edge) (delay edge +. lifetime)

propogateSignal go combine nodeId time graph = go [nodeId] times graph where
	times = combine (singleton (openRight time)) (findWithDef [nodeId] (nodes graph))

addSignal' :: (Ord nodeId, Ord time, Num time) => [nodeId] -> Life time -> Graph nodeId time -> Graph nodeId time
subSignal' :: (Ord nodeId, Ord time, Num time) => [nodeId] -> Life time -> Graph nodeId time -> Graph nodeId time
addSignal  :: (Ord nodeId, Ord time, Num time) =>  nodeId  ->      time -> Graph nodeId time -> Graph nodeId time
subSignal  :: (Ord nodeId, Ord time, Num time) =>  nodeId  ->      time -> Graph nodeId time -> Graph nodeId time

-- TODO: should subSignal' delete nodes when their lifetimes drop to zero?
addSignal' = propogateSignal' (M.insertWith union)
subSignal' = propogateSignal' (\path nodeDeath -> M.adjust (`diff` nodeDeath) path)
addSignal  = propogateSignal addSignal' diff
subSignal  = propogateSignal subSignal' intersect

addEdge'       :: (Ord nodeId, Ord time, Num time) => Life time -> nodeId -> nodeId -> Graph nodeId time -> Graph nodeId time
subEdge'       :: (Ord nodeId, Ord time, Num time) => Life time -> nodeId -> nodeId -> Graph nodeId time -> Graph nodeId time
addEdge        :: (Ord nodeId, Ord time, Num time) =>      time -> nodeId -> nodeId -> Graph nodeId time -> Graph nodeId time
subEdge        :: (Ord nodeId, Ord time, Num time) =>      time -> nodeId -> nodeId -> Graph nodeId time -> Graph nodeId time
initializeEdge :: (Ord nodeId, Ord time, Num time) =>      time -> nodeId -> nodeId -> Graph nodeId time -> Graph nodeId time

-- edge operations {{{1
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
	graph'   = graph { edges = adjust (\e -> maybe e id newEdge) source target (edges graph) }
	signals  = M.filterWithKey (\k _ -> take 1 k == [source]) (nodes graph)
	delayM   = maybe 0 delay newEdge -- the 0 should never matter, because anything using it will be thrown away
	mods     = recache source target : [mod (target:path) (intersect propLife (delayM +. lifetime)) | (path, lifetime) <- M.assocs signals]

addEdge' = propogateEdge' addSignal' diff      union       diff
subEdge' = propogateEdge' subSignal' intersect (flip diff) (flip diff)
addEdge  = unPrime addEdge'
subEdge  = unPrime subEdge'
unPrime f' time source target = f' (singleton (openRight time)) source target

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
instance Stable (Graph nodeId) where
	-- even though all the nodes may have stabilized, the entire graph may not have
	-- stabilized yet if there's still a signal traveling on its last leg in a
	-- cycle; to account for this, simply conservatively delay the stable time of
	-- the nodes by the maximal delay of any edge in the graph
	stable g = fmap (maxEdge g +) (maxNode g) where
		maxNode = mconcat . map stable . M.elems . nodes
		maxEdge = maximum . (0:) . map delay . concatMap M.elems . M.elems . edges

-- optimization idea: instead of recomputing the whole damn lifetime, just incrementally update it
recache :: (Ord nodeId, Ord time) => nodeId -> nodeId -> Graph nodeId time -> Graph nodeId time
recache source target graph = graph { edges = adjust update source target (edges graph) } where
	update  e = e { signalCache = signal }
	signal    = findWithDef source . M.mapKeysWith union head . M.filterWithKey valid . nodes $ graph
	valid k _ = take 1 k == [source] && take 1 (drop 1 k) /= [target]
	-- @valid@ makes sure we don't send a signal right back to the node it came from
