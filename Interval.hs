{-# LANGUAGE NoMonomorphismRestriction #-}
module Interval (
	Interval(..), start, end, unsafeStart, unsafeEnd,
	open, openLeft, openRight, closed,
	hasWidth, contains,
	NumLike(..),
	) where

import Bounds

import Control.Monad
import Data.Function
import Data.List (sortBy)
import Data.Maybe
import Data.Ord

newtype Interval a = Interval (AddMin a, AddMax a) deriving (Show, Read)
instance Functor Interval where fmap f (Interval (b, e)) = Interval (fmap f b, fmap f e)

instance Ord a => Eq (Interval a) where
	i == i' = on (&&) (not . hasWidth) i i' ||
	          start i == start i' && end i == end i'

instance Stable Interval where stable i = maybe (start i) return (unsafeEnd i)

start (Interval (b, e)) = b
end   (Interval (b, e)) = e

unsafeStart (Interval (AddMin b, AddMax e)) = b
unsafeEnd   (Interval (AddMin b, AddMax e)) = e

open          = Interval (mzero   , mzero   )
openLeft    e = Interval (mzero   , return e)
openRight b   = Interval (return b, mzero   )
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
	t *. (Interval (AddMin b, AddMax e)) = fmap (t*) (Interval (AddMin e, AddMax b))
	t /. (Interval (AddMin b, AddMax e)) = t *. fmap recip (Interval (AddMin e, AddMax b))
