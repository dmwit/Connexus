{-# LANGUAGE NoMonomorphismRestriction #-}
module Life (
	Life(), unLife,
	empty, singleton,
	isEmpty, contains,
	union, unions, diff, intersect, stripe,
	contiguous,
	NumLike(..)
	) where

import Bounds
import Interval (start, end, unsafeStart, unsafeEnd, hasWidth, Interval(..), NumLike(..))
import qualified Interval as I

import Control.Monad
import Data.Default
import Data.Function
import Data.List hiding (union, intersect)
import Data.Monoid
import Data.Ord

-- Invariants:
-- 1. no pair of intervals overlap
-- 2. the list is sorted in descending order of ending time
-- 3. all intervals have (positive) width
-- Together, (1) and (2) mean that for any sublist @i1:i2:is@, we have @end i2 <. start i1@.
newtype Life time = Life [Interval time] deriving (Eq, Read, Show)
instance Functor Life where fmap f (Life is) = Life (map (fmap f) is)
instance Default (Life a) where def = empty
unLife (Life is) = is

empty = Life []
singleton i = Life (strip [i])
isEmpty = (empty ==)

-- Invariants (1) and (2) ensure that if there are any intervals, the first one
-- has the highest stable time.
instance Stable Life where stable = mconcat . take 1 . map stable . unLife

-- Reinstate invariant (2). Preserves invariant (3).
reorder = sortBy (flip $ comparing end)

-- Given a list satisfying invariants (2) and (3), reinstate invariant (1).
collapse (i1 : i2 : is) | start i1 <=. end i2
	= collapse (Interval (on min start i1 i2, end i1) : is)
collapse (i:is) = i : collapse is
collapse [] = []

-- Reinstate invariant (1).
strip = filter hasWidth

-- When combining many lists, can drop from O(m * n^2) to O(m * n * log (m*n))
-- (where m is the number of lists and n is their average length) by doing a
-- sort rather than repeated merges.
unions = Life . collapse . reorder . concat . map unLife

-- When combining only two lists, can drop from O(n * log n) to O(n) (where n
-- is the average length of the arguments) by doing a merge rather than a sort.
-- When one of the lists is short, we can also win big by doing less collapsing
-- on the large list.
union a b = Life $ on go unLife a b where
	go is@(i:t) is'@(i':t')
		| end i > end i' = goAcc i  t is'
		| otherwise      = goAcc i' is t'
	go is []  = is
	go [] is' = is'

	goAcc acc is@(i:t) is'@(i':t')
		| end i > end i' = goCollapse acc i  t is'
		| otherwise      = goCollapse acc i' is t'
	goAcc acc [] is'@(i':t')
		| end i' <. start acc = acc : is'
		| otherwise           = goCollapse acc i' [] t'
	goAcc acc is@(i:t) []
		| end i  <. start acc = acc : is
		| otherwise           = goCollapse acc i t []
	goAcc acc [] [] = [acc]

	goCollapse i i' is is'
		| start i <=. end i' = goAcc (Interval (on min start i i', end i)) is is'
		| otherwise          = i : goAcc i' is is'

diff (Life is) (Life is') = Life (go is is') where
	go [] is = []
	go is [] = is
	go is@(i:t) is'@(i':t')
		| i' `I.contains`   i  = go t is'
		| start i >=. end   i' = i : go t is'
		| end   i <=. start i' = go is t'
		| start i <   start i' &&
		  end   i <=  end   i' = go (Interval (start i, reEnd i') : t) t'
		| start i <   start i' &&
		  end   i >   end   i' = Interval (reStart i', end i) : go (Interval (start i, reEnd i') : t) t'
		| end   i ==  end   i' = go t is'
		| otherwise            = Interval (reStart i', end i) : go t is'

	reStart = AddMin . unsafeEnd
	reEnd   = AddMax . unsafeStart

intersect  a = diff a . diff a
contiguous t = Life . takeWhile (\i -> return t <= end i) . dropWhile (\i -> return t < start i) . unLife
contains l t = not . isEmpty $ contiguous t l

stripe t (Life is)
	| t >= 0 = Life (strip    [Interval (fmap (+        t) (start i), end i) | i <- is])
	| t <  0 = Life (collapse [Interval (fmap (subtract t) (start i), end i) | i <- is])

instance NumLike Life where
	t +. l = fmap (t+) l
	t *. (Life is)
		| t  < 0 = Life . reverse  . map (t *.) $ is
		| t  > 0 = Life            . map (t *.) $ is
		| t == 0 = Life . collapse . map (t *.) $ is
	t /. (Life is) = t *. Life (map (1 /.) is)
