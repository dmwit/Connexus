{-# LANGUAGE NoMonomorphismRestriction #-}
module Misc where

import Control.Monad
import Control.Monad.Trans
import Data.Time

arbitraryUTCTime = UTCTime (ModifiedJulianDay 55000) (secondsToDiffTime 0)
time = liftIO $ fmap (fromRational . toRational . flip diffUTCTime arbitraryUTCTime) getCurrentTime
ignore    = (>> return ())
when_   b = when   b . ignore
unless_ b = unless b . ignore
