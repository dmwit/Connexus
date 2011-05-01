module Graph2 where

import Life

import Data.Map

data Graph nodeId time = Graph {
	edges :: Map (nodeId, nodeId) (Life time),
	nodes :: Map [nodeId] (Life time), -- no empty lists as keys
	delay :: Map (nodeId, nodeId) time
	} deriving (Eq, Ord, Show, Read)

query' :: (Ord nodeId, Ord time) => Graph nodeId time -> Map nodeId (Life time)
query  :: (Ord nodeId, Ord time) => Graph nodeId time -> nodeId -> Life time

query' = mapKeysWith Life.union head . delete [] {- defensive programming -} . nodes
-- TODO: are these acrobatics really necessary to get the necessary sharing?
query g = let q = query' g in \node -> findWithDefault Life.empty node q
