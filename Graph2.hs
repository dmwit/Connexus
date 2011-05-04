module Graph2 where

import Interval (openRight)
import Life

import Data.Default
import Data.Map (Map, adjust, assocs, delete, findWithDefault, insertWith, mapKeysWith)
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

findWithDef :: (Ord k, Default a) => k -> Map k a -> a
findWithDef = findWithDefault def

query' :: (Ord nodeId, Ord time) => Graph nodeId time -> Map nodeId (Life time)
query  :: (Ord nodeId, Ord time) => Graph nodeId time -> nodeId -> Life time

query' = mapKeysWith union head . delete [] {- defensive programming -} . nodes
-- TODO: are these acrobatics really necessary to get the necessary sharing?
query g = let q = query' g in \node -> findWithDef node q

propogateSignal' update path@(~(nodeId:rest)) lifetime graph
	| null path          = graph
	| isEmpty lifetime   = graph
	| nodeId `elem` rest = graph
	| otherwise          = foldr ($) graph' modifications where
	outgoing      = assocs (findWithDef nodeId (edges graph))
	modifications = [propogateSignal' update (nodeId':path) (shift edge) | (nodeId', edge) <- outgoing]
	graph'        = graph { nodes = update path lifetime (nodes graph) }
	shift edge    = intersect (propogation edge) (delay edge +. lifetime)

propogateSignal go combine nodeId time graph = go [nodeId] times graph where
	times = combine (singleton (openRight time)) (findWithDef [nodeId] (nodes graph))

addSignal' :: (Ord nodeId, Ord time, Num time) => [nodeId] -> Life time -> Graph nodeId time -> Graph nodeId time
addSignal  :: (Ord nodeId, Ord time, Num time) =>  nodeId  ->      time -> Graph nodeId time -> Graph nodeId time
subSignal' :: (Ord nodeId, Ord time, Num time) => [nodeId] -> Life time -> Graph nodeId time -> Graph nodeId time
subSignal  :: (Ord nodeId, Ord time, Num time) =>  nodeId  ->      time -> Graph nodeId time -> Graph nodeId time

addSignal' = propogateSignal' (insertWith union)
subSignal' = propogateSignal' (\path nodeDeath -> adjust (`diff` nodeDeath) path)
addSignal  = propogateSignal addSignal' diff
subSignal  = propogateSignal subSignal' intersect
