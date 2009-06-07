module Main where

import Control.Arrow
import Control.Monad.Random
import Control.Monad.State
import Control.Parallel.Strategies
import Control.Concurrent
import Data.Time
import Grid
import Physical
import System.IO

type Pos    = (Int, Int)
type SState = (Pos, Grid (Connection Pos))
type Synch  = MVar ()

riseAndShineThread :: Synch -> IO ()
riseAndShineThread ras = forever $ do
    tryPutMVar ras ()
    threadDelay 33333 -- 30 fps

riseAndShineAt :: Synch -> Time -> IO ()
riseAndShineAt    ras t = getTime >>= threadDelay . floor . (*1e6) . (t-) >> putMVar ras ()
riseAndShineAtAll ras ts = mapM_ (forkIO . riseAndShineAt ras) ts

baseUTCTime = UTCTime (ModifiedJulianDay 54832) 0
getTime = fmap (fromRational . toRational . flip diffUTCTime baseUTCTime) getCurrentTime

computationThread :: Synch -> MVar SState -> IO ()
computationThread ras s = forever $ do
    takeMVar ras
    t <- getTime
    (p, (ts, grid)) <- fmap (second . runState . update $ t) (takeMVar s)
    putMVar s (p, grid)
    rnf grid `seq` riseAndShineAtAll ras ts

inputThread :: Synch -> Synch -> MVar SState -> IO ()
inputThread rasCompute quit s = inputThread' True where
    inputThread' b = do
        c <- getChar
        case c of
            't' -> signal b >> inputThread' (not b)
            'h' -> move (-1)  0  b
            'j' -> move   0 (-1) b
            'k' -> move   0   1  b
            'l' -> move   1   0  b
            'q' -> putMVar quit ()
            _   -> inputThread' b
    signal b = do
        t <- getTime
        (p, grid) <- takeMVar s
        let (ts, grid') = runState (light t b (Point p)) grid
        putMVar s (p, grid')
        riseAndShineAtAll rasCompute ts
    move dx dy b = do
        unless b $ signal b
        modifyMVar_ s (\((x, y), grid) -> return ((x+dx, y+dy), grid))
        unless b $ signal (not b)
        inputThread' b

outputThread :: Synch -> MVar SState -> IO ()
outputThread ras s = forever $ do
    takeMVar ras
    t <- getTime
    (p, grid) <- readMVar s
    putStr (connectionPPrint t p grid)

initState :: IO SState
initState = fmap (((,) (1, 1)) . connectGrid . randomRectangular 5 4) newStdGen

main = do
    hSetBuffering stdin NoBuffering
    hSetEcho      stdin False
    rasCompute <- newMVar ()
    rasOutput  <- newMVar ()
    quit       <- newEmptyMVar
    s          <- initState >>= newMVar
    threads    <- mapM forkIO [riseAndShineThread rasOutput,
                               inputThread rasCompute quit s,
                               outputThread rasOutput s,
                               computationThread rasCompute s
                              ]
    takeMVar quit
    mapM_ killThread threads
