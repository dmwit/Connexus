-- boilerplate {{{
import Bounds
import Direction
import Grid2
import Lock
import Misc
import Viewport2

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
		= ioStateT lockRef
		-- TODO: this is horrible
		. whenUnlocked (\_ -> liftIO $ readIORef gridRef >>= rotate rotation pos >>= writeIORef gridRef)
		$ pos
	pos = (round x, round y)

drawGridLock gridRef lockRef = do
	liftIO (readIORef gridRef) >>= Grid2.update
	liftIO (readIORef lockRef) >>= Lock.update

main = do
	initGUI
	args    <- getArgs
	let (w, h) = case map reads args of
		[[(w, "")], [(h, "")]] -> (w, h)
		_ -> (11, 11)
	window  <- windowNew
	grid    <- randomGrid w h >>= rotateGridRandomly >>= signal (w `div` 2, h `div` 2)
	gridRef <- newIORef grid
	lockRef <- newIORef def
	da      <- viewportNew def {
		-- TODO: this is horrible
		stabilizationTime = liftM (maybe Already ExactTime . (\(AddMin v) -> v) . stable) (readIORef gridRef),
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
