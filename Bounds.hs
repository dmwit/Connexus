{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, StandaloneDeriving #-}
module Bounds (
	module Data.Monoid.Ord,
	MixOrd(..), Stable(..),
	(>.), (<.), (>=.), (<=.), (==.)
	) where

import Control.Monad
import Data.Monoid
import Data.Monoid.Ord

deriving instance Monad MaxPriority
deriving instance Monad MinPriority

class MixOrd a b where mixCompare :: a -> b -> Ordering
instance Ord a => MixOrd a a where mixCompare = compare
instance MixOrd a b => MixOrd (MaxPriority a) (MinPriority b) where
	mixCompare (MaxPriority Nothing) _ = LT
	mixCompare _ (MinPriority Nothing) = LT
	mixCompare (MaxPriority (Just a)) (MinPriority (Just b)) = mixCompare a b
instance MixOrd b a => MixOrd (MinPriority a) (MaxPriority b) where
	mixCompare a b = compare EQ (mixCompare b a)

x >.  y = mixCompare x y == GT
x <.  y = mixCompare x y == LT
x >=. y = mixCompare x y /= LT
x <=. y = mixCompare x y /= GT
x ==. y = mixCompare x y == EQ

class Stable f where
	stable :: (Ord time, Num time) => f time -> MaxPriority time
