{-# LANGUAGE FlexibleContexts #-}
module Graph (Edge(..), Graph, edges, signalGraph, addNode, addEdge, startEdge, endEdge, queryNode, queryEdge, stable) where

import Interval
import Path

import Control.Monad.State
import Data.Default
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Edge nodeId time = Edge {
	source      :: nodeId,
	target      :: nodeId,
	delay       :: time,
	lifetime    :: Interval time
	} deriving (Eq, Ord, Show, Read)

data Node nodeId edgeId time = Node {
	outgoing    :: Set edgeId,
	history     :: History nodeId edgeId time
	} deriving (Eq, Ord, Show, Read)

data Graph nodeId edgeId time = Graph {
	nextNodeId  :: nodeId,
	nextEdgeId  :: edgeId,
	edges       :: Map edgeId (Edge nodeId        time),
	nodes       :: Map nodeId (Node nodeId edgeId time)
	} deriving (Eq, Ord, Show, Read)

instance Default (Node nodeId edgeId time) where def = Node def def
instance (Bounded nodeId, Bounded edgeId) => Default (Graph nodeId edgeId time) where
	def = Graph minBound minBound def def

signalNode   :: (Ord nodeId, Ord edgeId)                     =>          Path nodeId edgeId -> Interval time  -> Node  nodeId edgeId time -> ([edgeId], Node  nodeId edgeId time)
signalEdge   :: (Eq  nodeId,             Ord time, Num time) =>          Path nodeId edgeId -> Interval time  -> Edge  nodeId        time -> ( nodeId , Interval            time)
signalGraph' :: (Ord nodeId, Ord edgeId, Ord time, Num time) => (nodeId, Path nodeId edgeId,   Interval time) -> Graph nodeId edgeId time ->            Graph nodeId edgeId time

refresh      :: (MonadState (Graph nodeId edgeId time) m, Ord nodeId, Ord edgeId,              Ord time, Num time) => nodeId ->                           m ()
signalGraph  :: (MonadState (Graph nodeId edgeId time) m, Ord nodeId, Ord edgeId,              Ord time, Num time) => nodeId -> Interval time          -> m ()
addNode      :: (MonadState (Graph nodeId edgeId time) m, Ord nodeId,             Enum nodeId                    ) =>                                     m nodeId
addEdge      :: (MonadState (Graph nodeId edgeId time) m, Ord nodeId, Ord edgeId, Enum edgeId, Ord time, Num time) => nodeId -> nodeId -> time         -> m edgeId
startEdge    :: (MonadState (Graph nodeId edgeId time) m, Ord nodeId, Ord edgeId, Enum edgeId, Ord time, Num time) => nodeId -> nodeId -> time -> time -> m edgeId
endEdge      :: (MonadState (Graph nodeId edgeId time) m, Ord nodeId, Ord edgeId,              Ord time, Num time) => edgeId ->                   time -> m ()
startEdge'   :: (MonadState (Graph nodeId edgeId time) m, Ord nodeId, Ord edgeId, Enum edgeId, Ord time, Num time) => nodeId -> nodeId -> time -> Maybe time -> m edgeId

queryNode :: (Ord nodeId, Ord edgeId, Ord time, Num        time) => nodeId ->         Graph nodeId edgeId time -> [Interval time]
queryEdge :: (Ord nodeId, Ord edgeId, Ord time, Fractional time) => edgeId -> time -> Graph nodeId edgeId time -> [Interval time]

maybeMaximum   :: Ord time => [Maybe time] -> Maybe time
stableInterval :: Ord time => Interval               time -> Maybe time
stableEdge     :: Ord time => Edge     nodeId        time -> Maybe time
stableHistory  :: Ord time => History  nodeId edgeId time -> Maybe time
stableNode     :: Ord time => Node     nodeId edgeId time -> Maybe time
stable         :: Ord time => Graph    nodeId edgeId time -> Maybe time

signalNode path signal node = (edges, newNode) where
	edges    = Set.toList (outgoing node)
	newNode  = node { history = updateHistory path signal (history node) }

signalEdge path signal edge = (target edge, transmit signal) where
	clip     = intersect (lifetime edge)
	transmit = clip . (delay edge +.) . clip

lookupList :: Ord k => k -> Map k v -> [v]
lookupList k = maybe [] (:[]) . Map.lookup k

signalGraph' (nodeId, path, signal) graph = foldr signalGraph' graph' nextSignals where
	atNodeId f nodeId graph@(Graph { nodes = ns }) = case Map.lookup nodeId ns of
		Just node -> let (v, node') = f node in (v, graph { nodes = Map.insert nodeId node' ns })
		Nothing   -> ([], graph)

	(edgeIds, graph') = (signalNode path signal `atNodeId` nodeId) graph
	nextSignals       = [ (nodeId', appendPath nodeId edgeId path, signal')
	                    | edgeId <- edgeIds
	                    , edge   <- lookupList edgeId (edges graph)
	                    , not (target edge `elemPath` path) -- break cycles
	                    , let (nodeId', signal') = signalEdge path signal edge
	                    ]

-- optimization idea: this function is called when an edge changes; instead of
-- sending all signals along all edges, just signal along the edge that changes
refresh nodeId = maybe (return ()) signalRefreshAll . Map.lookup nodeId =<< gets nodes
	where
	signalRefresh (path, signal) = modify . signalGraph' $ (nodeId, path, signal)
	signalRefreshAll             = mapM_ signalRefresh . listHistory . history

signalGraph nodeId signal = modify $ signalGraph' (nodeId, def, signal)

addNode = do
	graph@(Graph { nextNodeId =      n, nodes = ns }) <- get
	put    graph { nextNodeId = succ n, nodes = Map.insert n def ns }
	return n

lookupM k m f = maybe (return ()) f . Map.lookup k $ m

startEdge' edgeSource edgeTarget edgeDelay mTime = do
	graph@(Graph { nextEdgeId = e, edges = es, nodes = ns }) <- get
	lookupM edgeSource ns $ \node -> do
		put graph {
			nextEdgeId = succ e,
			edges      = Map.insert e newEdge es,
			nodes      = Map.insert edgeSource (newNode e node) ns
			}
		refresh edgeSource
	return e
	where
	newEdge = Edge { source = edgeSource, target = edgeTarget, delay = edgeDelay, lifetime = maybe open openRight mTime }
	newNode edgeId node = node { outgoing = Set.insert edgeId (outgoing node) }

startEdge edgeSource edgeTarget edgeDelay = startEdge' edgeSource edgeTarget edgeDelay . Just
addEdge   edgeSource edgeTarget edgeDelay = startEdge' edgeSource edgeTarget edgeDelay Nothing

endEdge edgeId time = do
	graph@(Graph { edges = es }) <- get
	lookupM edgeId es $ \edge -> do
		put graph { edges = Map.insert edgeId (newEdge edge) es }
		refresh (source edge)
	where
	newEdge edge@(Edge { lifetime = Interval (b, e) }) = edge { lifetime = Interval (b, Just time) }

queryNode nodeId      graph = union $ lookupList nodeId (nodes graph) >>= map snd . listHistory . history
queryEdge edgeId time graph = union $ do
	edge <- lookupList edgeId        (edges graph)
	node <- lookupList (source edge) (nodes graph)
	(path, signal) <- listHistory (history node)
	guard . not $ lastPath (target edge) path -- don't send signals back to the node they came from instantly
	return . intersect (closed 0 1) $ (time -. intersect (lifetime edge) signal) ./ delay edge

maybeMaximum xs = case catMaybes xs of
	[] -> Nothing
	xs -> Just (maximum xs)

stableInterval (Interval (b, e)) = maybeMaximum [b, e]
stableEdge    = stableInterval . lifetime
stableHistory = maybeMaximum . map (stableInterval . snd) . listHistory
stableNode    = stableHistory . history
stable      g = maybeMaximum (stableEdges ++ stableNodes) where
	stableEdges = map stableEdge . Map.elems . edges $ g
	stableNodes = map stableNode . Map.elems . nodes $ g
