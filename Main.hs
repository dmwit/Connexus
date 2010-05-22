-- boilerplate {{{
import Direction
import Grid hiding (width, height)
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
-- }}}
clickGrid gridRef lockRef b x y = case b of
	LeftButton   -> doRotation counterclockwise
	RightButton  -> doRotation clockwise
	MiddleButton -> ioStateT lockRef (toggle pos)
	where
	doRotation rotation
		= ioStateT lockRef
		. whenUnlocked (ioStateT gridRef . rotate rotation)
		$ pos
	pos = (round x, round y)

drawGridLock gridRef lockRef = do
	gridRender <- readIORef gridRef >>= Grid.update
	lock       <- readIORef lockRef
	return (gridRender >> Lock.update lock)

main = do
	initGUI
	window  <- windowNew
	grid    <- evalStateT (randomGrid 11 11) def
	gridRef <- newIORef grid
	lockRef <- newIORef def
	ioStateT gridRef $ rotateGridRandomly >> time >>= signal (5, 5) . (+2)
	da      <- viewportNew def {
		stabilizationTime = liftM (maybe Already ExactTime . stable) (readIORef gridRef),
		draw     = drawGridLock gridRef lockRef,
		click    = clickGrid    gridRef lockRef,
		position = Position {
			centerX = def { dimension =  5 }, centerY = def { dimension =  5 },
			width   = def { dimension = 11 }, height  = def { dimension = 11 }
			}
		}
	set window [containerChild := da]
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI

-- TODO:
-- (among others)
-- * clicking near the border on connections that extend out of their box
-- * somehow indicate the boundaries between the pieces / grid spaces
-- * do something when they win =)
-- * don't lock off-grid locations
-- * make it easier to change the size of the grid
