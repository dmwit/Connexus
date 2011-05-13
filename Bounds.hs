{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
module Bounds where

import Control.Monad
import Data.Monoid

newtype AddMax a = AddMax (Maybe a) deriving (Eq, Show, Read, Functor, Monad, MonadPlus)
newtype AddMin a = AddMin (Maybe a) deriving (Eq, Show, Read, Functor, Monad, MonadPlus, Ord {- Nothing < Just x -})

instance Ord a => Ord (AddMax a) where
	AddMax (Just x) `compare` AddMax (Just y) = compare x y
	AddMax x        `compare` AddMax y        = compare (AddMin y) (AddMin x)

class MixOrd a b where mixCompare :: a -> b -> Ordering
instance Ord a => MixOrd a a where mixCompare = compare
instance MixOrd a b => MixOrd (AddMin a) (AddMax b) where
	mixCompare (AddMin Nothing) _ = LT
	mixCompare _ (AddMax Nothing) = LT
	mixCompare (AddMin (Just a)) (AddMax (Just b)) = mixCompare a b
instance MixOrd b a => MixOrd (AddMax a) (AddMin b) where
	mixCompare a b = compare EQ (mixCompare b a)

instance Ord a => Monoid (AddMax a) where mempty = mzero; mappend = min
instance Ord a => Monoid (AddMin a) where mempty = mzero; mappend = max

x >.  y = mixCompare x y == GT
x <.  y = mixCompare x y == LT
x >=. y = mixCompare x y /= LT
x <=. y = mixCompare x y /= GT
x ==. y = mixCompare x y == EQ

class Stable f where
	stable :: (Ord time, Num time) => f time -> AddMin time
