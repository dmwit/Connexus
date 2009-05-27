module Main where

import Control.Monad.State
import Control.Parallel.Strategies
import Control.Concurrent
import Data.Time
import Grid
import Physical
import System.IO

type SState = Grid (Int, Int)
type Synch  = MVar ()

riseAndShineThread :: Synch -> IO ()
riseAndShineThread ras = forever $ do
    tryPutMVar ras ()
    threadDelay 33333 -- 30 fps

riseAndShineAt :: Synch -> Time -> IO ()
riseAndShineAt    ras t = getTime >>= threadDelay . floor . (*1e6) . (t-) >> putMVar ras ()
riseAndShineAtAll ras ts = mapM (forkIO . riseAndShineAt ras) ts

baseUTCTime = UTCTime (ModifiedJulianDay 54832) 0
getTime = fmap (fromRational . toRational . flip diffUTCTime baseUTCTime) getCurrentTime

computationThread :: Synch -> MVar SState -> IO ()
computationThread ras s = forever $ do
    takeMVar ras
    t          <- getTime
    (ts, grid) <- fmap (runState (update t)) (takeMVar s)
    putMVar s grid
    rnf grid `seq` riseAndShineAtAll ras ts

inputThread :: Synch -> Synch -> MVar SState -> IO ()
inputThread rasCompute quit s = inputThread' True where
    inputThread' b = do
        c <- getChar
        case c of
            't' -> do
                t          <- getTime
                (ts, grid) <- fmap (runState (light t b (0, 0))) (takeMVar s)
                putMVar s grid
                riseAndShineAtAll rasCompute ts
                inputThread' (not b)
            'q' -> putMVar quit ()
            _   -> inputThread' b

outputThread :: Synch -> MVar SState -> IO ()
outputThread ras s = forever $ do
    takeMVar ras
    grid <- readMVar s
    t    <- getTime
    putStr (basicPPrint t grid)

main = do
    hSetBuffering stdin NoBuffering
    hSetEcho      stdin False
    rasCompute <- newMVar ()
    rasOutput  <- newMVar ()
    quit       <- newEmptyMVar
    s          <- newMVar . basicGrid $ [((x, y), (x+1, y)) | x <- [0..2], y <- [0..2]] ++ [((3, 1), (3, 2)), ((3, 0), (3, 1)), ((0, 0), (0, 1))]
    threads    <- mapM forkIO [riseAndShineThread rasOutput,
                               inputThread rasCompute quit s,
                               outputThread rasOutput s,
                               computationThread rasCompute s
                              ]
    takeMVar quit
    mapM_ killThread threads
