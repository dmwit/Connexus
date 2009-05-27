-- boilerplate {{{
{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module Edge where

import Control.Arrow
import Control.Monad.Reader
import Control.Monad.State
import Control.Parallel.Strategies
import Data.Function
import Data.List (groupBy)
import Data.Map (Map)
import Data.Maybe
import Physical

import qualified Data.Map as Map
-- }}}
-- EdgeClass {{{
class EdgeClass e where
    -- controller
    signal   :: Signal -> State e [Time]           -- queue an input; returns times at which new outputs will be available
                                                   -- different signal sources should use different sequence numbers!
    collect  :: Time   -> State e [Signal]         -- gather available outputs; do any garbage collection associated with fully propogated signals
                                                   -- collect should never return the same "on" signal twice unless there was a corresponding "off" signal in between
                                                   -- note the asymmetry: it is considered acceptable behavior to propogate the same "off" signal twice in succession
                                                   -- (Yes, this means that this is not really a physical model, since an edge participating in a loop will not propogate looped signals.  This is on purpose.)
    destroy  :: Time   -> State e [Signal]         -- end any pending outputs

    -- view
    transit  ::           e -> Time                -- report the end-to-end signal travel time
    coverage :: Time   -> e -> [Interval Time]     -- report which bits of [0, transit[ are currently "on"

    -- default implementations
    destroy t = fmap (endSignals t) (collect t)
-- }}}
-- DirectedEdge {{{
data DirectedEdge = DirectedEdge {
    lastCollection :: Maybe Time,
    statuses       :: Map Status Time,
    deTransit      :: Time
} deriving (Eq, Ord, Show, Read)

mStatuses f = modify (\de -> de { statuses = f (statuses de) })
sStatuses   = mStatuses . const
mLastCollection f = modify (\de -> de { lastCollection = f (lastCollection de) })
sLastCollection   = mLastCollection . const

directedEdge = DirectedEdge Nothing Map.empty
toSignals transit = return . Map.assocs . Map.map (+transit)
endSignals t signals = [((n, False), t) | ((n, _), _) <- signals]

instance EdgeClass DirectedEdge where
    signal (status, t) = do
        transit_ <- gets transit
        oldT     <- gets (Map.lookup status . statuses)
        mStatuses (Map.insertWith min status t)
        return [t + transit_ | maybe True (t<) oldT]

    -- old signals are ready to propogate to neighbors
    -- young signals have not yet reached the other end of the edge
    -- dead signals are old signals for which a corresponding "off" signal is ready to propogate
    -- live signals are old signals which have not yet been terminated by a propogated "off" signal
    collect t = do
        transit_     <- gets transit
        mLastT       <- gets lastCollection
        (old, young) <- gets (Map.partition (<= t - transit_) . statuses)
        let (dead, live) = Map.partitionWithKey (\(seqNo, _) _ -> (seqNo, False) `Map.member` old) old
        sStatuses (young `Map.union` live)
        sLastCollection . Just . maximum . catMaybes $ [Just (t - transit_), mLastT]
        toSignals transit_ (maybe id (Map.filter . (<)) mLastT old)

    transit = deTransit

    coverage t (DirectedEdge { deTransit = transit_, statuses = statuses_ }) = distanceIntervals where
        toInterval [((_, False), te), ((_, True), tb)] = [closed te tb]
        toInterval [((_, True), tb)]                   = [openLeft  tb]
        toInterval _                                   = [            ]
        timeIntervals     = groupBy ((==) `on` (fst . fst)) (Map.toAscList statuses_) >>= toInterval
        timeToDistance    = intersect (closed 0 transit_) . fmap (t -)
        distanceIntervals = union . map timeToDistance $ timeIntervals

instance NFData DirectedEdge where
    rnf de = rnf (statuses de)
-- }}}
-- Edge {{{
data Edge = forall e. (EdgeClass e, Show e, NFData e) => Edge { unEdge :: e }

edgeLift :: (forall e. (EdgeClass e) => State e a) -> State Edge a
edgeLift m = State (\(Edge e) -> second Edge (runState m e))

instance EdgeClass Edge where
    signal  s = edgeLift (signal  s)
    collect t = edgeLift (collect t)
    destroy t = edgeLift (destroy t)

    transit    (Edge e) = transit e
    coverage t (Edge e) = coverage t e

instance Show Edge where
    show (Edge e) = "Edge (" ++ show e ++ ")"

instance NFData Edge where
    rnf (Edge e) = rnf e
-- }}}
