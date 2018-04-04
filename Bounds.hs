{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, StandaloneDeriving #-}
module Bounds (
	module Data.Monoid.Inf,
	inf, swapInfty,
	MixOrd(..), Stable(..),
	(>.), (<.), (>=.), (<=.), (==.)
	) where

import Data.Monoid.Inf hiding (minimum, maximum)

instance Applicative (Inf p) where
	pure = Finite
	Finite f <*> Finite x = Finite (f x)
	_ <*> _ = Infinity

instance Monad (Inf p) where
	return x = Finite x
	Infinity >>= f = Infinity
	Finite v >>= f = f v
	fail s = Infinity

inf v f Infinity = v
inf v f (Finite v') = f v'
swapInfty = inf Infinity Finite

class MixOrd a b where mixCompare :: a -> b -> Ordering
instance Ord a => MixOrd a a where mixCompare = compare
instance MixOrd a b => MixOrd (NegInf a) (PosInf b) where
	mixCompare Infinity _ = LT
	mixCompare _ Infinity = LT
	mixCompare (Finite a) (Finite b) = mixCompare a b
instance MixOrd b a => MixOrd (PosInf a) (NegInf b) where
	mixCompare a b = compare EQ (mixCompare b a)

x >.  y = mixCompare x y == GT
x <.  y = mixCompare x y == LT
x >=. y = mixCompare x y /= LT
x <=. y = mixCompare x y /= GT
x ==. y = mixCompare x y == EQ

class Stable f where
	stable :: (Ord time, Num time) => f time -> NegInf time
