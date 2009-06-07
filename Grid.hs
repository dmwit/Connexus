-- boilerplate {{{
{-# LANGUAGE NoMonomorphismRestriction #-}

module Grid (
    Grid(..),
    addEdge,
    basicGrid,
    emptyGrid,
    basicPPrint,
    connectionPPrint,
    light,
    update,
    RandomGrid,
    expand,
    randomRectangular,
    runRandomGrid,
    Connection(..),
    connectGrid
) where

import qualified Data.Map as Map
import qualified PriorityQueue as PQ

import Control.Arrow
import Control.Monad.Random
import Control.Monad.State
import Control.Parallel.Strategies
import Data.List (delete, foldl', nub, transpose)
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Edge
import Physical
import PriorityQueue (PriorityQueue)

adjustWithDefault def f k m = Map.alter (Just . maybe (f def) f) k m where
-- }}}
-- Grid data type, instances, and basic creation {{{
type Edges i = Map i (Map i Edge)
data Grid i = Grid {
    sequenceNumber :: SequenceNumber,
    edges          :: Edges i,
    updates        :: PriorityQueue Time (i, i)
} deriving Show

mEdges f g = g { edges = f (edges g) }
sEdges     = mEdges . const
speed = 20

instance NFData i => NFData (Grid i) where
    rnf g = rnf (sequenceNumber g, edges g, updates g)

addEdge :: Ord k => Edge -> k -> k -> Edges k -> Edges k
addEdge edge i j = adjustWithDefault Map.empty (Map.insert j edge) i
simpleEdge       = Edge $ directedEdge (1 / speed)
addSimpleEdge    = addEdge simpleEdge

-- we can use PQ.empty because we know no DirectedEdge will start out with a
-- pending "on" signal
basicGrid :: Ord i => [(i, i)] -> Grid i
basicGrid pairs = Grid minBound edges_ PQ.empty where
    symmetricPairs = pairs ++ map (snd &&& fst) pairs
    edges_         = foldr (uncurry addSimpleEdge) Map.empty symmetricPairs

emptyGrid = Grid minBound Map.empty PQ.empty
-- }}}
-- for debugging {{{
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

intersections  is js = map (\i -> any (\j -> not . empty $ intersect i j) js) is
intervals        e s = map (uncurry closed) . ap zip tail $ [0, 1/(speed * s) .. transit e]
representation t e s = intersections (intervals e s) (coverage t e)

hLink edges_ s t = (>>= replicate 2) . linkRep hRep edges_ s t
vLink = linkRep vRep
linkRep rep edges_ s t l = case getLink l edges_ of
    Nothing       -> replicate (floor s) ' '
    Just (ef, eb) -> zipWith rep (representation t ef s) (reverse $ representation t eb s)

-- assumptions: only symmetric single-step NSEW connections
basicPPrint :: Integral i => Time -> Grid (i, i) -> String
basicPPrint t grid | Map.null (edges grid) = ""
                   | otherwise = all where
    s      = 8
    edges_ = edges grid
    nodes  = Map.keys edges_
    ((minX, minY), (maxX, maxY)) = foldl' (\((minX, minY), (maxX, maxY)) (x, y) -> ((min minX x, min minY y), (max maxX x, max maxY y))) (head nodes, head nodes) nodes -- this line wasn't long enough yet, don't you agree?

    row     y = [minX..maxX] >>= \x -> nodeRep (Map.member (x, y) edges_) : hLink edges_ s t ((x, y), (x+1, y))
    columns y = transpose $ do
        x <- [minX..maxX]
        vLink edges_ s t ((x, y), (x, y-1)) : replicate (2*s) (replicate s ' ')

    all = unlines [l | y <- [maxY, maxY-1 .. minY], l <- row y : columns y]

-- assumptions: lots
connectionPPrint :: Integral i => Time -> (i, i) -> Grid (Connection (i, i)) -> String
connectionPPrint t c grid | Map.null (edges grid) = ""
                          | otherwise = all where
    s      = 4
    edges_ = edges grid
    nodes  = Map.keys edges_

    p = case head nodes of Point i -> i; Connection i _ -> i
    combine ((minX, minY), (maxX, maxY)) p = case p of
        Point (x, y) -> ((min minX x, min minY y), (max maxX x, max maxY y))
        Connection (x1, y1) (x2, y2) -> ((minimum [minX, x1, x2], minimum [minY, y1, y2]),
                                         (maximum [maxX, x1, x2], maximum [maxY, y1, y2]))
    ((minX, minY), (maxX, maxY)) = foldl' combine (p, p) nodes

    node x y =  vEdge (Connection (x, y) (x, y+1)) (Point (x, y))
             ++ [   hEdge (Connection (x-1, y) (x, y)) (Point (x, y))
                 ++ (if (x, y) == c then '*' else nodeRep (Map.member (Point (x, y)) edges_))
                 :  hEdge (Point (x, y)) (Connection (x, y) (x+1, y))
                ]
             ++ vEdge (Point (x, y)) (Connection (x, y-1) (x, y))
        where
        padding = replicate (2*s) (replicate s ' ')
        vEdge i1 i2 = transpose $ padding ++ vLink edges_ s t (i1, i2) : padding
        hEdge = curry (hLink edges_ s t)

    row y = unlines . map concat . transpose . map (flip node y) $ [minX..maxX]
    all = [maxY, maxY-1 .. minY] >>= row
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
        liftM (concat tss ++) (update t)
-- }}}
-- creating a random tree {{{
type RandomGrid g i a = StateT ([i], Grid i) (Rand g) a

uniform [] = return Nothing
uniform xs = liftM Just $ unsafeUniform xs
unsafeUniform = fromList . flip zip (repeat 1)

nodeEmpty i = maybe True Map.null . Map.lookup i . edges
link    i j = modify . second . mEdges $ addSimpleEdge i j . addSimpleEdge j i
bernoulli p = fmap (<p) (getRandomR (0.0, 1.0))

chooseLeaf = gets fst >>= uniform
chooseEmptyNode is = uniform =<< return . flip filter is . flip nodeEmpty =<< gets snd

expand :: (RandomGen g, Ord i) => (i -> [i]) -> RandomGrid g i ()
expand neighbors = loop where
    don't = return ()
    loop  = do
        mi <- chooseLeaf
        case mi of
            Nothing -> don't
            Just i  -> do
                mj <- chooseEmptyNode (neighbors i)
                maybe don't (link i) mj
                modify . first . maybe (delete i) (:) $ mj
                loop

randomRectangular w h = runRandomGrid (expand neighbors) (topHalf w, topHalf h) where
    topHalf x        = (x + 1) `div` 2
    nearby x w       = [x + dx | dx <- [-1, 1], 0 < x + dx, x + dx < w + 1]
    neighbors (x, y) = map (flip (,) y) (nearby x w) ++ map ((,) x) (nearby y h)

runRandomGrid :: RandomGen g => RandomGrid g i a -> i -> g -> Grid i
runRandomGrid m i = evalRand . fmap snd $ execStateT m ([i], emptyGrid)
-- }}}
-- Connection {{{
data Connection a = Point a | Connection a a deriving (Read, Show)

-- The order of the arguments to Connection shouldn't matter for anything real;
-- only which two things are being connected matter.
liftConnection combine op a b c d = (min a b `op` min c d) `combine` (max a b `op` max c d)

instance Ord a => Eq (Connection a) where
    (Point a) == (Point b) = a == b
    (Connection a b) == (Connection c d) = liftConnection (&&) (==) a b c d
    _ == _ = False

instance Ord a => Ord (Connection a) where
    compare (Point a) (Point b)        = compare a b
    compare (Point {}) (Connection {}) = LT
    compare (Connection {}) (Point {}) = GT
    compare (Connection a b) (Connection c d) = liftConnection mappend compare a b c d

instance Functor Connection where
    fmap f (Point      a  ) = Point      (f a)
    fmap f (Connection a b) = Connection (f a) (f b)

instance NFData a => NFData (Connection a) where
    rnf (Point      a  ) = rnf a
    rnf (Connection a b) = rnf a `seq` rnf b

connectEdges es = foldr (uncurry . uncurry $ addEdge) Map.empty assocs where
    assocs = do
        (i1, i2e) <- Map.assocs es
        (i2,   e) <- Map.assocs i2e
        [((e, Point i1), Connection i1 i2), ((e, Connection i1 i2), Point i2)]

-- does two strange things: duplicates each edge, and starts the signal queue empty
connectGrid g = g { edges = connectEdges (edges g), updates = PQ.empty }
-- }}}
