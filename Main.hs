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
import System.Environment
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
	args    <- getArgs
	let (w, h) = case map reads args of
		[[(w, "")], [(h, "")]] -> (w, h)
		_ -> (11, 11)
	window  <- windowNew
	grid    <- evalStateT (randomGrid w h) def
	gridRef <- newIORef grid
	lockRef <- newIORef def
	ioStateT gridRef $ rotateGridRandomly >> time >>= signal (w `div` 2, h `div` 2)
	da      <- viewportNew def {
		stabilizationTime = liftM (maybe Already ExactTime . stable) (readIORef gridRef),
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

-- TODO:
-- (among others)
-- * clicking near the border on connections that extend out of their box
-- * somehow indicate the boundaries between the pieces / grid spaces
-- * do something when they win =)
-- * don't lock off-grid locations
-- * make it easier to change the size of the grid
-- * keyboard bindings
-- * non-idiot mode: don't face edges outside
-- * countably infinite puzzle
-- * pipe dream mode
-- * make drag require Ctrl or something so that middle-click is less annoying
-- * wrapping/klein bottle mode
-- * two-sided puzzle with links from one layer to the other or something
