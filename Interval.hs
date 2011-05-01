{-# LANGUAGE NoMonomorphismRestriction #-}
module Interval (
	Interval(..), start, end,
	open, openLeft, openRight, closed,
	isEmpty, isPoint, contains, hasPoint, overlaps,
	intersect, union,
	(.+), (.-), (.*), (./), (+.), (-.), (*.), (/.)
	) where

import Control.Monad
import Data.Function
import Data.List (sortBy)
import Data.Monoid
import Data.Ord

-- Beware: these Eq and Ord instances are not meant to be meaningful! (In
-- particular, they may do strange things with empty intervals.) TODO: fix this
-- (at least for the Eq instance) by making all empty intervals equal.
newtype Interval a = Interval (Maybe a, Maybe a) deriving (Eq, Ord, Show, Read)
instance Functor Interval where fmap f (Interval (b, e)) = Interval (fmap f b, fmap f e)

start (Interval (b, e)) = b
end   (Interval (b, e)) = e

open          = Interval (Nothing, Nothing)
openLeft    e = Interval (Nothing, Just  e)
openRight b   = Interval (Just  b, Nothing)
closed    b e = Interval (Just  b, Just  e)

isEmpty (Interval (Just b, Just e)) = b > e
isEmpty _ = False

isPoint (Interval (Just b, Just e)) = b == e
isPoint _ = False

-- Beware: this may do strange things with empty intervals.
i1 `contains` i2 = intersect i1 i2 == i2
i1 `overlaps` i2 = not . isEmpty $ i1 `intersect` i2
i  `hasPoint` p  = i `contains` closed p p

-- maybe2 is designed to do minimal allocation, which can make a huge
-- difference in runtime (and maybe2 is one of the top bottlenecks)
maybe2 f Nothing     = id
maybe2 f mx@(Just x) = maybe mx (Just . f x)

intersect (Interval (b1, e1)) (Interval (b2, e2)) =
	Interval (maybe2 max b1 b2, maybe2 min e1 e2)

-- TODO: remove these from Interval, leaving them only in Life
-- Nothing < Just x
union = unsafeUnion . sortBy (comparing start) . filter (not . isEmpty)

unsafeUnion (i1@(Interval (b1, e1)) : i2@(Interval (_, e2)) : is)
	-- optimization idea: if e2 is Nothing, can return immediately
	| i1 `overlaps` i2 = unsafeUnion (Interval (b1, liftM2 max e1 e2) : is)
	| otherwise        = i1 : unsafeUnion (i2 : is)
unsafeUnion is = is

-- the dot goes on the unusual (i.e. the Interval) side
infixl 6 .+
infixl 6 +.
infixl 6 .-
infixl 6 -.
infixl 7 .*
infixl 7 *.
infixl 7 ./
infixl 7 /.

i .+ t = t +. i
i .* t = t *. i
i .- t = fmap (subtract t) i
i ./ t = i .* recip t
t +. i = fmap (t+) i
t -. (Interval (b, e)) = fmap (t-) (Interval (e, b))
t *. i | t >= 0        = fmap (t*) i
t *. (Interval (b, e)) = fmap (t*) (Interval (e, b))
t /. (Interval (b, e)) = t *. fmap recip (Interval (e, b))
