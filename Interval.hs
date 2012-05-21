{-# LANGUAGE NoMonomorphismRestriction #-}
module Interval (
	Interval(..), start, end, unsafeStart, unsafeEnd,
	open, openLeft, openRight, closed,
	hasWidth, contains,
	NumLike(..),
	) where

import Bounds
import Misc

import Data.Function
import Data.Monoid

newtype Interval a = Interval (MaxPriority a, MinPriority a) deriving (Show, Read)
instance Functor Interval where fmap f (Interval (b, e)) = Interval (fmap f b, fmap f e)

instance Ord a => Eq (Interval a) where
	i == i' = on (&&) (not . hasWidth) i i' ||
	          start i == start i' && end i == end i'

instance Stable Interval where stable i = maybe (start i) return (unsafeEnd i)

instance PPrint a => PPrint (MaxPriority a) where pprint (MaxPriority n) = maybe "-infty" pprint n
instance PPrint a => PPrint (MinPriority a) where pprint (MinPriority n) = maybe  "infty" pprint n
instance PPrint a => PPrint (Interval    a) where pprint (Interval    i) = pprint i

start (Interval (b, e)) = b
end   (Interval (b, e)) = e

unsafeStart (Interval (MaxPriority b, MinPriority e)) = b
unsafeEnd   (Interval (MaxPriority b, MinPriority e)) = e

open          = Interval (mempty  , mempty  )
openLeft    e = Interval (mempty  , return e)
openRight b   = Interval (return b, mempty  )
closed    b e = Interval (return b, return e)

hasWidth i      = start i <. end i
i `contains` i' = start i <= start i' && end i >= end i'

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
	t *. (Interval (MaxPriority b, MinPriority e)) = fmap (t*) (Interval (MaxPriority e, MinPriority b))
	t /. (Interval (MaxPriority b, MinPriority e)) = t *. fmap recip (Interval (MaxPriority e, MinPriority b))
