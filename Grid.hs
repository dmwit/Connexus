-- boilerplate {{{
{-# LANGUAGE NoMonomorphismRestriction #-}

module Grid where

import qualified Data.Map as Map
import qualified PriorityQueue as PQ

import Control.Arrow
import Control.Monad.Random
import Control.Monad.State
import Control.Parallel.Strategies
import Data.List (foldl', nub, transpose)
import Data.Map (Map)
import Data.Maybe
import Edge
import Physical
import PriorityQueue (PriorityQueue)

adjustWithDefault def f k m = Map.alter (Just . maybe (f def) f) k m where
-- }}}
-- Grid data type, instances, and basic creation {{{
data Grid i = Grid {
    sequenceNumber :: SequenceNumber,
    edges          :: Map i (Map i Edge),
    updates        :: PriorityQueue Time (i, i)
} deriving Show

instance NFData i => NFData (Grid i) where
    rnf g = rnf (sequenceNumber g, edges g, updates g)

addEdge edge i j = adjustWithDefault Map.empty (Map.insert j edge) i
simpleEdge       = Edge $ directedEdge 1
addSimpleEdge    = addEdge simpleEdge

-- we can use PQ.empty because we know no DirectedEdge will start out with a
-- pending "on" signal
basicGrid pairs = Grid minBound edges_ PQ.empty where
    symmetricPairs = pairs ++ map (snd &&& fst) pairs
    edges_         = foldr (uncurry addSimpleEdge) Map.empty symmetricPairs
-- }}}
-- for debugging {{{
-- assumptions: only single-step NSEW connections
basicPPrint t grid | Map.null (edges grid) = ""
                   | otherwise = all where
    s :: Num a => a
    s      = 8
    edges_ = edges grid
    nodes  = Map.keys edges_
    ((minX, minY), (maxX, maxY)) = foldl' (\((minX, minY), (maxX, maxY)) (x, y) -> ((min minX x, min minY y), (max maxX x, max maxY y))) (head nodes, head nodes) nodes -- this line wasn't long enough yet, don't you agree?

    intersections is js = map (\i -> any (\j -> not . empty $ intersect i j) js) is
    intervals        e  = map (uncurry closed) . ap zip tail $ [0, 1/s .. transit e]
    representation t e  = intersections (intervals e) (coverage t e)

    hRep False False = '-'
    hRep False True  = '<'
    hRep True  False = '>'
    hRep True  True  = 'x'

    vRep False False = '|'
    vRep False True  = '^'
    vRep True  False = 'v'
    vRep True  True  = 'x'

    nodeRep False = ' '
    nodeRep True  = '+'

    hLink = (>>= replicate 2) . link hRep
    vLink = link vRep
    link rep l = case getLink l edges_ of
        Nothing       -> replicate s ' '
        Just (ef, eb) -> zipWith rep (representation t ef) (reverse $ representation t eb)

    row     y = [minX..maxX] >>= \x -> nodeRep (Map.member (x, y) edges_) : hLink ((x, y), (x+1, y))
    columns y = transpose $ do
        x <- [minX..maxX]
        vLink ((x, y), (x, y-1)) : replicate (2*s) (replicate s ' ')

    all = unlines [l | y <- [maxY, maxY-1 .. minY], l <- row y : columns y]
-- }}}
-- internal operations {{{
getSingleLink (a, b) es = Map.lookup a es >>= Map.lookup b
getLink       (a, b) es = liftM2 (,) (getSingleLink (a, b) es) (getSingleLink (b, a) es)

putSingleLink (a, b) e = modify (\g@Grid { edges = es } -> g { edges = adjustWithDefault Map.empty (Map.insert b e) a es })

atLink m i = do
    (a, e) <- gets (runState m . fromJust . getSingleLink i . edges)
    putSingleLink i e
    return a

enqueue i j t = modify (\g -> g { updates = PQ.enqueue t (i, j) (updates g) }) >> return t
dequeue       = modify (\g -> g { updates = PQ.dequeue          (updates g) })
signalLink s l@(i, j) = signal s `atLink` l >>= mapM (enqueue i j)

signalNode avoid node signal = do
    neighbors <- gets (maybe [] (filter (/= avoid) . Map.keys) . Map.lookup node . edges)
    tss <- mapM (\neighbor -> signalLink signal (node, neighbor)) neighbors
    return (concat tss)
-- }}}
-- external API {{{
light t p i = do
    seqNo     <- gets sequenceNumber
    neighbors <- gets (fmap Map.keys . Map.lookup i . edges)
    ts        <- forM (fromMaybe [] neighbors) $ \i' -> signalLink ((seqNo, p), t) (i, i')
    unless p (modify (\g -> g { sequenceNumber = sequenceNumber g + 1 }))
    return (nub $ concat ts)

update t = do
    pq <- gets updates
    (\c -> maybe (return []) c (PQ.peekTo t pq)) $ \l@(i, j) -> do
        dequeue
        signals <- collect t `atLink` l
        tss     <- mapM (signalNode i j) signals
        fmap (concat tss ++) (update t)
-- }}}
-- creating a random tree {{{
type RandomGrid g i a = StateT (Grid i) (Rand g) a

uniform = fromList . flip zip (repeat 1)
nodeEmpty i = gets (maybe True Map.null . Map.lookup i . edges)
-- }}}
debug1 = basicGrid [p | [x, y] <- replicateM 2 [0..2], p <- [((x, y), (x+1, y)), ((x, y), (x, y+1))]]
debug2 = execState (light 0   True  (1, 1)) debug1
debug3 = execState (light 0.5 False (1, 1)) debug2
debug4 = execState (update 1.5) debug3
debug5 = execState (update 2.5) debug4
debug6 = execState (update 2.5) debug3
