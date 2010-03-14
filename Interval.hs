{-# LANGUAGE NoMonomorphismRestriction #-}
module Interval (
	Interval(..),
	open, openLeft, openRight, closed,
	isEmpty, contains, hasPoint, overlaps,
	intersect, union,
	(.+), (.-), (.*), (./), (+.), (-.), (*.), (/.)
	) where

import Empty

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

list2 :: Maybe a -> Maybe a -> Maybe [a]
list2 = mappend `on` fmap return

-- Beware: this may do strange things with empty intervals.
i1 `contains` i2 = intersect i1 i2 == i2
i1 `overlaps` i2 = not . isEmpty $ i1 `intersect` i2
i  `hasPoint` p  = i `contains` closed p p

intersect (Interval (b1, e1)) (Interval (b2, e2)) =
	Interval (fmap maximum (list2 b1 b2), fmap minimum (list2 e1 e2))

-- Nothing < Just x
union = coalesce . sortBy (comparing start) . filter (not . isEmpty) where
	coalesce (i1@(Interval (b1, e1)) : i2@(Interval (_, e2)) : is)
		-- optimization idea: if e2 is Nothing, can return immediately
		| i1 `overlaps` i2 = coalesce (Interval (b1, liftM2 max e1 e2) : is)
		| otherwise        = i1 : coalesce (i2 : is)
	coalesce is = is

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
