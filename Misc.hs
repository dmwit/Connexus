{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Misc where

import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Trans
import Data.Array.Base
import Data.Array.IO
import Data.Array.MArray
import Data.Default
import Data.IntMap
import Data.IORef
import Data.Time
import System.Random

arbitraryUTCTime = UTCTime (ModifiedJulianDay 55000) (secondsToDiffTime 0)
time = liftIO $ fmap (fromRational . toRational . flip diffUTCTime arbitraryUTCTime) getCurrentTime
ignore    = (>> return ())
when_   b = when   b . ignore
unless_ b = unless b . ignore
ioStateT sRef m = do
	s <- liftIO (readIORef sRef)
	(a, s') <- runStateT m s
	liftIO (writeIORef sRef s')
	return a

lift1 f = lift . f
lift2 f = (lift .) . f
lift3 f = ((lift .) .) . f
instance MArray IOArray e m => MArray IOArray e (StateT s m) where
	getBounds       = lift1 getBounds
	getNumElements  = lift1 getNumElements
	newArray        = lift2 newArray
	newArray_       = lift1 newArray_
	unsafeNewArray_ = lift1 unsafeNewArray_
	unsafeRead      = lift2 unsafeRead
	unsafeWrite     = lift3 unsafeWrite

instance (Random a, Random b) => Random (a, b) where
	randomR ((alo, blo), (ahi, bhi)) g = let
		(a, g' ) = randomR (alo, ahi) g
		(b, g'') = randomR (blo, bhi) g'
		in ((a, b), g'')
	random g = let (a, g') = random g; (b, g'') = random g' in ((a, b), g'')

instance Default (IntMap a) where def = empty
