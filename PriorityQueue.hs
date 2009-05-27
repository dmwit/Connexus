{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
module PriorityQueue (PriorityQueue, empty, isEmpty, enqueue, fromList, peek, dequeue, peekDequeue, peekTo) where

import Control.Arrow
import Control.Parallel.Strategies
import Data.List
import Data.Maybe

newtype PriorityQueueEntry k v = PQE { unPQE :: (k, v) } deriving (Show, Read, NFData)
newtype PriorityQueue      k v = PQ  { unPQ  :: [PriorityQueueEntry k v] } deriving (Eq, Ord, Show, Read, NFData)

instance Eq  k => Eq  (PriorityQueueEntry k v) where
    (PQE (k1, _)) == (PQE (k2, _)) = k1 == k2

instance Ord k => Ord (PriorityQueueEntry k v) where
    compare (PQE (k1, _)) (PQE (k2, _)) = compare k1 k2

empty   = PQ []
isEmpty = null . unPQ
enqueue k v = PQ . insert (PQE (k, v)) . unPQ

fromList = PQ . sort . map PQE
peek     = fmap snd . listToMaybe . map unPQE . unPQ
dequeue  = PQ . drop 1 . unPQ

peekDequeue (PQ []) = Nothing
peekDequeue (PQ ((PQE (_, v)):xs)) = Just (v, PQ xs)

peekTo k (PQ ((PQE (k', v)):xs)) | k' <= k = Just v
peekTo _ _ = Nothing
