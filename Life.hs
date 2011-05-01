{-# LANGUAGE NoMonomorphismRestriction #-}
module Life (
	Life, canonize,
	empty, singleton, union
	) where

import Interval hiding (union)

import Control.Monad
import Data.List hiding (union)
import Data.Ord

newtype Life time = Life [Interval time] deriving (Eq, Ord, Read, Show)
instance Functor Life where fmap f (Life is) = Life (map (fmap f) is)
unLife (Life is) = is

empty = Life []
singleton i = canonize (Life [i])

-- Nothing < Just x
canonize
	= Life
	. collapse
	. sortBy (flip (comparing start))
	. filter (not . isEmpty)
	. unLife

collapse (i1@(Interval (b1, e1)) : i2@(Interval (_, e2)) : is)
	-- optimization idea: if e2 is Nothing, can return immediately
	| i1 `overlaps` i2 = collapse (Interval (b1, liftM2 max e1 e2) : is)
	| otherwise        = i1 : collapse (i2 : is)
collapse is = is

union (Life is) (Life is') = Life . collapse $ interleave is is' where
	interleave (i:is) (i':is')
		| start i < start i' = i' : interleave (i:is) is'
		| otherwise          = i  : interleave is (i':is')
	interleave is is' = is ++ is'

diff (Life is) (Life is') = Life (go is is') where
	go [] is = []
	go is [] = is
	go is@(i:t) is'@(i':t')
		| start i > end i' = i : go t is'
		| otherwise = undefined
