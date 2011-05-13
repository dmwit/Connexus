{-# LANGUAGE NoMonomorphismRestriction #-}
module Interval (
	Interval(..), start, end, unsafeStart, unsafeEnd,
	open, openLeft, openRight, closed,
	isEmpty, isPoint, hasWidth,
	contains, hasPoint, overlaps,
	intersect, union,
	NumLike(..),
	) where

import Bounds

import Control.Monad
import Data.List (sortBy)
import Data.Maybe
import Data.Ord

-- Beware: these Eq and Ord instances are not meant to be meaningful! (In
-- particular, they may do strange things with empty intervals.) TODO: fix this
-- (at least for the Eq instance) by making all empty intervals equal.
newtype Interval a = Interval (AddMin a, AddMax a) deriving (Eq, Ord, Show, Read)
instance Functor Interval where fmap f (Interval (b, e)) = Interval (fmap f b, fmap f e)

start (Interval (b, e)) = b
end   (Interval (b, e)) = e

unsafeStart (Interval (AddMin b, AddMax e)) = b
unsafeEnd   (Interval (AddMin b, AddMax e)) = e

open          = Interval (mzero   , mzero   )
openLeft    e = Interval (mzero   , return e)
openRight b   = Interval (return b, mzero   )
closed    b e = Interval (return b, return e)

isEmpty  i = start i >.  end i
isPoint  i = start i ==. end i
hasWidth i = start i <.  end i

instance Stable Interval where stable i = maybe (start i) return (unsafeEnd i)

-- Beware: this may do strange things with empty intervals.
i1 `contains` i2 = intersect i1 i2 == i2
i1 `overlaps` i2 = not . isEmpty $ i1 `intersect` i2
i  `hasPoint` p  = i `contains` closed p p

intersect (Interval (b1, e1)) (Interval (b2, e2)) =
	Interval (max b1 b2, min e1 e2)

-- TODO: remove these from Interval, leaving them only in Life
union = unsafeUnion . sortBy (comparing start) . filter (not . isEmpty)

unsafeUnion (i1@(Interval (b1, e1)) : i2@(Interval (_, e2)) : is)
	-- optimization idea: if e2 is Nothing, can return immediately
	| i1 `overlaps` i2 = unsafeUnion (Interval (b1, liftM2 max e1 e2) : is)
	| otherwise        = i1 : unsafeUnion (i2 : is)
unsafeUnion is = is

infixl 6 .+
infixl 6 +.
infixl 6 .-
infixl 6 -.
infixl 7 .*
infixl 7 *.
infixl 7 ./
infixl 7 /.

class NumLike f where
	-- the dot goes on the unusual (i.e. the Interval) side
	(.+), (.-), (.*) :: (Ord a, Num a) => f a -> a -> f a
	(+.), (-.), (*.) :: (Ord a, Num a) => a -> f a -> f a
	(./) :: (Ord a, Fractional a) => f a -> a -> f a
	(/.) :: (Ord a, Fractional a) => a -> f a -> f a

	f .+ a = a +. f
	f .* a = a *. f
	f .- a = f .+ negate a
	f ./ a = f .* recip a
	a +. f = f .+ a
	a *. f = f .* a
	a -. f = a +. (-1) *. f

instance NumLike Interval where
	t +. i = fmap (t+) i
	t *. i | t >= 0 = fmap (t*) i
	t *. (Interval (AddMin b, AddMax e)) = fmap (t*) (Interval (AddMin e, AddMax b))
	t /. (Interval (AddMin b, AddMax e)) = t *. fmap recip (Interval (AddMin e, AddMax b))
