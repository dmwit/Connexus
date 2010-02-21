module Path where

import Interval

import Data.Map (Map)
import qualified Data.Map as Map

-- optimization ideas
-- * type Path nodeId edgeId = ([(edgeId, nodeId)], Set nodeId)
-- * type History nodeId edgeId time = Trie (edgeId, nodeId) (Interval time)
type Path    nodeId edgeId      = [(edgeId, nodeId)] -- program invariant: ap (==) nub (map snd path)
type History nodeId edgeId time = Map (Path nodeId edgeId) (Interval time)

appendPath :: nodeId -> edgeId -> Path nodeId edgeId -> Path nodeId edgeId
elemPath   :: Eq nodeId => nodeId -> Path nodeId edgeId -> Bool
lastPath   :: Eq nodeId => nodeId -> Path nodeId edgeId -> Bool
listPath   :: Path nodeId edgeId -> [(edgeId, nodeId)]

updateHistory :: (Ord nodeId, Ord edgeId) =>
	Path nodeId edgeId -> Interval time -> History nodeId edgeId time -> History nodeId edgeId time
listHistory   :: (Ord nodeId, Ord edgeId) =>
	History nodeId edgeId time -> [(Path nodeId edgeId, Interval time)]

appendPath node edge path = (edge, node) : path
elemPath   node      path = elem node (map snd path)
lastPath   node      path = case listPath path of
	((_, node'):_) -> node == node'
	[]             -> False
listPath = id

updateHistory = Map.insert
listHistory   = Map.assocs
