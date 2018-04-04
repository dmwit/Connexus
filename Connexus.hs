-- boilerplate {{{1
{-# LANGUAGE MonoLocalBinds #-}
import Bounds
import Direction
import Grid
import Lock
import Misc
import Viewport

import Control.Monad
import Control.Monad.Reader
import Data.Set (Set)
import Data.Default
import Data.IORef
import Graphics.UI.Gtk hiding (Viewport, viewportNew)
import System.Environment
-- }}}

clickGrid grid lockRef b x y = case b of
	LeftButton   -> doRotation counterclockwise
	RightButton  -> doRotation clockwise
	MiddleButton -> ioStateT lockRef (toggle pos)
	where
	doRotation rotation
		= ioStateT lockRef . whenUnlocked pos . liftIO
		$ rotate rotation pos grid
	pos = (round x, round y)

drawGridLock grid lockRef = do
	Grid.update grid
	liftIO (readIORef lockRef) >>= Lock.update

main = do
	args    <- initGUI
	let (w, h) = case map reads args of
	    	[[(w, "")], [(h, "")]] -> (w, h)
	    	_ -> (11, 11)
	window  <- windowNew
	grid    <- randomGrid w h
	rotateGridRandomly grid
	signal (w `div` 2, h `div` 2) grid
	--grid    <- staticGrid ([((0,y),[North,East,South]) | y <- [0..10]] ++ [((1,y),[North,West,South]) | y <- [0..10]])
	--signal (0,0) grid
	lockRef <- newIORef def
	da      <- viewportNew def {
		stabilizationTime = liftM (fromStableTime . stable) (readIORef (graph grid)),
		draw     = \_ -> drawGridLock grid lockRef,
		click    =       clickGrid    grid lockRef,
		position = Position {
			centerX = def { dimension = (fromIntegral w - 1) / 2 }, centerY = def { dimension = (fromIntegral h - 1) / 2 },
			width   = def { dimension =  fromIntegral w          }, height  = def { dimension =  fromIntegral h          }
			}
		}
	set window [containerChild := da]
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI
