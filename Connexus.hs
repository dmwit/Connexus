-- boilerplate {{{
import Bounds
import Direction
import Grid
import Lock
import Misc
import Viewport

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Default
import Data.IORef
import Graphics.UI.Gtk hiding (Viewport, viewportNew)
import Graphics.UI.Gtk.Gdk.EventM (MouseButton(..))
import System.Environment
-- }}}
clickGrid gridRef lockRef b x y = case b of
	LeftButton   -> doRotation counterclockwise
	RightButton  -> doRotation clockwise
	MiddleButton -> ioStateT lockRef (toggle pos)
	where
	doRotation rotation
		= ioStateT lockRef . whenUnlocked pos . liftIO
		$ readIORef gridRef >>= rotate rotation pos >>= writeIORef gridRef
	pos = (round x, round y)

drawGridLock gridRef lockRef = do
	liftIO (readIORef gridRef) >>= Grid.update
	liftIO (readIORef lockRef) >>= Lock.update

main = do
	initGUI
	args    <- getArgs
	let (w, h) = case map reads args of
		[[(w, "")], [(h, "")]] -> (w, h)
		_ -> (11, 11)
	window  <- windowNew
	grid    <- randomGrid w h >>= rotateGridRandomly >>= signal (w `div` 2, h `div` 2)
	--grid    <- unsafeStaticGrid ([((0,y),[North,East,South]) | y <- [0..10]] ++ [((1,y),[North,West,South]) | y <- [0..10]]) >>= signal (0,0)
	gridRef <- newIORef grid
	lockRef <- newIORef def
	da      <- viewportNew def {
		stabilizationTime = liftM (fromStableTime . stable) (readIORef gridRef),
		draw     = drawGridLock gridRef lockRef,
		click    = clickGrid    gridRef lockRef,
		position = Position {
			centerX = def { dimension = (fromIntegral w - 1) / 2 }, centerY = def { dimension = (fromIntegral h - 1) / 2 },
			width   = def { dimension =  fromIntegral w          }, height  = def { dimension =  fromIntegral h          }
			}
		}
	set window [containerChild := da]
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI
