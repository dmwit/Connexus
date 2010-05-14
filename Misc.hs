{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Misc where

import Control.Monad.State
import Control.Monad.Trans
import Data.Array.Base
import Data.Array.IO
import Data.Array.MArray
import Data.IORef
import Data.Time

arbitraryUTCTime = UTCTime (ModifiedJulianDay 55000) (secondsToDiffTime 0)
time = liftIO $ fmap (fromRational . toRational . flip diffUTCTime arbitraryUTCTime) getCurrentTime
ignore    = (>> return ())
when_   b = when   b . ignore
unless_ b = unless b . ignore
ioStateT m sRef = do
	s <- liftIO (readIORef sRef)
	(a, s') <- runStateT m s
	liftIO (writeIORef sRef s')
	return a

lift1 f = liftIO . f
lift2 f = (liftIO .) . f
lift3 f = ((liftIO .) .) . f
instance MArray IOArray e IO => MArray IOArray e (StateT s IO) where
	getBounds       = lift1 getBounds
	getNumElements  = lift1 getNumElements
	newArray        = lift2 newArray
	newArray_       = lift1 newArray_
	unsafeNewArray_ = lift1 unsafeNewArray_
	unsafeRead      = lift2 unsafeRead
	unsafeWrite     = lift3 unsafeWrite
