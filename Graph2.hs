{-# LANGUAGE NoImplicitPrelude #-}

module Graph2 where

import Interval (openRight)
import Life

import Control.Monad
import Data.Default
import Data.Map (Map)
import Prelude hiding (lookup)
import qualified Data.Map as M

data Edge time = Edge {
	delay :: time,
	life  :: Life time
	} deriving (Eq, Ord, Show, Read)

-- TODO: cache this in Edge itself?
propogation e = stripe (delay e) (life e)

data Graph nodeId time = Graph {
	edges :: Map nodeId (Map nodeId (Edge time)),
	nodes :: Map [nodeId] (Life time) -- no empty lists as keys
	} deriving (Eq, Ord, Show, Read)
instance Default (Graph nodeId time) where def = Graph def def

findWithDef :: (Ord k, Default a) => k -> Map k a -> a
findWithDef = M.findWithDefault def

insert :: (Ord k1, Ord k2) => k1 -> k2 -> v -> Map k1 (Map k2 v) -> Map k1 (Map k2 v)
lookup :: (Ord k1, Ord k2) => k1 -> k2 -> Map k1 (Map k2 v) -> Maybe v
adjust :: (Ord k1, Ord k2) => (v -> v) -> k1 -> k2 -> Map k1 (Map k2 v) -> Map k1 (Map k2 v)

insert   k1 k2 v = flip (M.unionWith (M.unionWith (flip const))) (M.singleton k1 (M.singleton k2 v))
lookup   k1 k2   = M.lookup k1 >=> M.lookup k2
adjust f k1 k2   = M.adjust (M.adjust f k2) k1

query' :: (Ord nodeId, Ord time) => Graph nodeId time -> Map nodeId (Life time)
query  :: (Ord nodeId, Ord time) => Graph nodeId time -> nodeId -> Life time

query' = M.mapKeysWith union head . M.delete [] {- defensive programming -} . nodes
query g = let q = query' g in \node -> findWithDef node q
-- written that way so that q is shared across multiple calls with varying values of "node"

propogateSignal' update path@(~(nodeId:rest)) lifetime graph
	| null path          = graph
	| isEmpty lifetime   = graph
	| nodeId `elem` rest = graph
	| otherwise          = foldr ($) graph' modifications where
	outgoing      = M.assocs (findWithDef nodeId (edges graph))
	modifications = [propogateSignal' update (nodeId':path) (shift edge) | (nodeId', edge) <- outgoing]
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

addEdge'       :: (Ord nodeId, Ord time, Num time) => nodeId -> nodeId -> Life time -> Graph nodeId time -> Graph nodeId time
subEdge'       :: (Ord nodeId, Ord time, Num time) => nodeId -> nodeId -> Life time -> Graph nodeId time -> Graph nodeId time
addEdge        :: (Ord nodeId, Ord time, Num time) => nodeId -> nodeId ->      time -> Graph nodeId time -> Graph nodeId time
subEdge        :: (Ord nodeId, Ord time, Num time) => nodeId -> nodeId ->      time -> Graph nodeId time -> Graph nodeId time
initializeEdge :: (Ord nodeId, Ord time, Num time) => nodeId -> nodeId ->      time -> Graph nodeId time -> Graph nodeId time

propogateEdge' mod overlap combine newProp source target edgeLife graph = foldr ($) graph' mods where
	oldEdge  = lookup source target (edges graph)
	newLife  = maybe empty (overlap edgeLife . life) oldEdge
	newEdge  = fmap (\e -> e { life = combine newLife (life e) }) oldEdge
	propM    = maybe empty propogation
	propLife = newProp (propM newEdge) (propM oldEdge)
	graph'   = graph { edges = adjust (\e -> maybe e id newEdge) source target (edges graph) }
	signals  = M.filterWithKey (\k _ -> take 1 k == [source]) (nodes graph)
	delayM   = maybe 0 delay newEdge -- the 0 should never matter, because anything using it will be thrown away
	mods     = [mod (target:path) (intersect propLife (delayM +. lifetime)) | (path, lifetime) <- M.assocs signals]

addEdge' = propogateEdge' addSignal' diff      union       diff
subEdge' = propogateEdge' subSignal' intersect (flip diff) (flip diff)
addEdge  = unPrime addEdge'
subEdge  = unPrime subEdge'
unPrime f' source target = f' source target . singleton . openRight

initializeEdge source target delay graph = insertInto cleanGraph where
	oldEdge    = lookup source target (edges graph)
	cleanGraph = maybe id (subEdge' source target . life) oldEdge graph
	insertInto = \g -> g { edges = insert source target (Edge delay empty) (edges g) }
