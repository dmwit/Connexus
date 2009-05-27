{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies #-}
module Physical (Time, Distance, Speed, c,
                 (!/), (!*),
                 Interval, open, openLeft, openRight, closed,
                 empty, intersect, union, contains,
                 SequenceNumber, Status, Signal ) where

import Control.Monad (liftM2)
import Control.Parallel.Strategies
import Data.List     (sort)
import Data.Monoid   (mconcat)
import Data.Ratio    (Rational)

newtype Time     = Time     { unTime     :: Rational } deriving (Eq, Ord, Show, Read, Num, Real, Fractional, Enum, RealFrac)
newtype Distance = Distance { unDistance :: Rational } deriving (Eq, Ord, Show, Read, Num, Real, Fractional, Enum, RealFrac)
newtype Speed    = Speed    { unSpeed    :: Rational } deriving (Eq, Ord, Show, Read, Num, Real, Fractional, Enum, RealFrac)

c = 1 :: Speed

class Multiplicative a b c | a b -> c where (!*) :: a -> b -> c
class Divisive       a b c | a b -> c where (!/) :: a -> b -> c

liftRational op a b = fromRational (toRational a `op` toRational b)
liftMonoid   op a b = fmap op . mconcat . map (fmap return) $ [a, b]

instance Multiplicative Speed Time Distance where (!*) = liftRational (*)
instance Multiplicative Time Speed Distance where (!*) = liftRational (*)
instance Divisive       Distance Speed Time where (!/) = liftRational (/)
instance Divisive       Distance Time Speed where (!/) = liftRational (/)

instance NFData Time     where rnf = (`seq` ())
instance NFData Distance where rnf = (`seq` ())
instance NFData Speed    where rnf = (`seq` ())

newtype Interval a = Interval (Maybe a, Maybe a) deriving (Eq, Ord, Show, Read)
instance Functor Interval where fmap f (Interval (b, e)) = Interval (fmap f b, fmap f e)

open          = Interval (Nothing, Nothing)
openLeft    e = Interval (Nothing, Just  e)
openRight b   = Interval (Just  b, Nothing)
closed    b e = Interval (Just  b, Just  e)

intersect (Interval (b, e)) (Interval (b', e')) = Interval
    (liftMonoid maximum b b', liftMonoid minimum e e')
contains i i' = intersect i i' == i

empty (Interval (Just b, Just e)) = b > e
empty _ = False

sortedUnion (i@(Interval (b, e)) : is@(Interval (b', e') : is'))
    | b' <= e   = sortedUnion (Interval (b, iMax e e') : is')
    | otherwise = i : sortedUnion is
    where iMax a b = a >> b >> liftM2 max a b
sortedUnion otherwise = otherwise

union = filter (not . empty) . sortedUnion . sort

type SequenceNumber = Int
type Status = (SequenceNumber, Bool)
type Signal = (Status, Time)
