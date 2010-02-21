-- boilerplate {{{
{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
import Empty
import Graph
import Interval

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Data.Time
import Data.Word
import System.Exit

newtype ReaderMVarT s m a = ReaderMVarT (ReaderT (MVar s) m a) deriving (MonadReader (MVar s), MonadIO, Monad)
runReaderMVarT (ReaderMVarT m) v = runReaderT m v

instance MonadIO m => MonadState s (ReaderMVarT s m) where
	get = do
		mvar  <- ask
		value <- liftIO (takeMVar mvar)
		liftIO (putMVar mvar value)
		return value
	put value = do
		mvar <- ask
		liftIO (takeMVar mvar >> putMVar mvar value)
-- }}}
arbitraryUTCTime = UTCTime (ModifiedJulianDay 55000) (secondsToDiffTime 0)
time = liftIO $ fmap (toRational . flip diffUTCTime arbitraryUTCTime) getCurrentTime

constructGraph = do
	[driver, a, b, c, d] <- replicateM 5 addNode
	now   <- time
	edges <- mapM (\(source, target) -> startEdge source target 2 now) [
		(a, b), (b, c), (c, d), (d, b),
		(b, a), (c, b), (d, c), (b, d)
		]
	signalGraph driver open
	return ((driver, a), edges)

keyboardThread driver a = go Nothing where
	go e = liftIO getChar >>= \c -> case c of
		't' -> case e of
			Nothing -> time >>= startEdge driver a 0 >>= go . Just
			Just e  -> time >>= endEdge e >> go Nothing
		'q' -> put (error "Time to go") >> liftIO (exitWith ExitSuccess)
		_ -> go e

forkReader m = ask >>= liftIO . forkIO . runReaderMVarT m

showIntervals l is js = [showIntervalParts l n is js | n <- [0..l-1]]
showIntervalParts l n is js = case (forward, backward) of
	(False, False) -> ' '
	(False, True ) -> '<'
	(True , False) -> '>'
	(True , True ) -> 'x'
	where
	forward  = any (`contains` closed (n/l) ((n+1)/l)) is
	backward = any (`contains` closed ((l-n-1)/l) ((l-n)/l)) js

displaySingle edges now forth back name = do
	liftIO . putStr $ name ++ " "
	is <- go forth
	js <- go back
	liftIO . putStrLn $ showIntervals 10 is js
	where go i = gets $ queryEdge (edges !! i) now

displayThread edges = do
	now <- time
	mapM_ (uncurry (uncurry (displaySingle edges now))) [
		((0, 4), "ab"),
		((1, 5), "bc"),
		((2, 6), "cd"),
		((3, 7), "db")
		]
	liftIO (threadDelay 33333 >> putChar '\n')

main = newMVar (empty :: Graph Word64 Word64 Rational) >>= runReaderMVarT (do
	(forKeyboard, forDisplay) <- constructGraph
	forkReader (uncurry keyboardThread forKeyboard)
	forever (displayThread forDisplay)
	)
