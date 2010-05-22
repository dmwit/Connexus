-- boilerplate {{{
import Direction
import Grid hiding (width, height)
--import Grid (graph, rotate, unsafeStaticGrid)
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

testGraph = [((0, 0), [South, East]), ((1, 0), [West, South, East]), ((2, 0), [West]), ((0, 1), [North, East]), ((1, 1), [East, North, West]), ((2, 1), [West])]

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

{-
testGraph = [((0, 0), [South, East]), ((1, 0), [West, South, East]), ((2, 0), [West]), ((0, 1), [North, East]), ((1, 1), [East, North, West]), ((2, 1), [West])]

main = do
	grid    <- unsafeStaticGrid testGraph
	gridRef <- newIORef grid
	ioStateT gridRef $ replicateM_ 160 (rotate clockwise (1, 0))
	readIORef gridRef >>= putStrLn . take 1 . reverse . show . graph
-}
