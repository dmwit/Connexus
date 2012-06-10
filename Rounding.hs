{-# LANGUAGE NoMonomorphismRestriction #-}
module Rounding where

{-# INLINE towardZero          #-}
{-# INLINE towardInf           #-}
{-# INLINE towardNegInf        #-}
{-# INLINE awayFromZero        #-}
{-# INLINE nearestTowardZero   #-}
{-# INLINE nearestTowardInf    #-}
{-# INLINE nearestTowardNegInf #-}
{-# INLINE nearestAwayFromZero #-}
{-# INLINE nearestBanker       #-}

towardZero     = truncate
towardInf      = ceiling
towardNegInf   = floor
awayFromZero v = if v > 0 then ceiling v else floor v

nearestTowardZero   v = if isHalf v then towardZero   v else round v
nearestTowardInf    v = if isHalf v then towardInf    v else round v
nearestTowardNegInf v = if isHalf v then towardNegInf v else round v
nearestAwayFromZero v = if isHalf v then awayFromZero v else round v
nearestBanker         = round

{-# INLINE fi #-}
{-# INLINE isHalf #-}
fi = fromIntegral
isHalf v = v - fromInteger (towardNegInf v) == 0.5
