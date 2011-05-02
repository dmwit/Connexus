module Graph2 where

import Interval (openRight)
import Life

import Data.Default
import Data.Map

data Edge time = Edge {
	delay :: time,
	life  :: Life time
	} deriving (Eq, Ord, Show, Read)

data Graph nodeId time = Graph {
	edges :: Map nodeId (Map nodeId (Edge time)),
	nodes :: Map [nodeId] (Life time) -- no empty lists as keys
	} deriving (Eq, Ord, Show, Read)

findWithDef :: (Ord k, Default a) => k -> Map k a -> a
findWithDef = findWithDefault def

query' :: (Ord nodeId, Ord time) => Graph nodeId time -> Map nodeId (Life time)
query  :: (Ord nodeId, Ord time) => Graph nodeId time -> nodeId -> Life time

query' = mapKeysWith Life.union head . delete [] {- defensive programming -} . nodes
-- TODO: are these acrobatics really necessary to get the necessary sharing?
query g = let q = query' g in \node -> findWithDef node q

addSignal' :: (Ord nodeId, Ord time, Num time) => [nodeId] -> Life time -> Graph nodeId time -> Graph nodeId time
addSignal  :: (Ord nodeId, Ord time, Num time) =>  nodeId  ->      time -> Graph nodeId time -> Graph nodeId time

addSignal' [] is g = g
addSignal' ns@(n:t) is g
	| isEmpty is  = g
	| n `elem` t  = g
	| otherwise   = foldr ($) g' modifications where
	outgoing      = assocs (findWithDef n (edges g))
	modifications = [addSignal' (n':ns) (shift (life e) (delay e)) | (n', e) <- outgoing]
	g'            = g { nodes = insertWith Life.union ns is (nodes g) }
	shift l d     = intersect (d +. intersect is l) l

addSignal n t g = addSignal' [n] newlife g where
	newlife = diff (Life.singleton (openRight t)) (findWithDef [n] (nodes g))
