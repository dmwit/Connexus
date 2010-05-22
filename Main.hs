import Direction
import Grid (randomGrid, rotate, signal, stable, update)
--import Grid (graph, rotate, unsafeStaticGrid)
import Misc
import Viewport

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Default
import Data.IORef
import Graphics.UI.Gtk hiding (Viewport, viewportNew)
import Graphics.UI.Gtk.Gdk.EventM (MouseButton(..))

clickGrid gridRef b x y = ioStateT go gridRef where
	go       = rotate rotation (round x, round y)
	rotation = case b of
		LeftButton  -> counterclockwise
		RightButton -> clockwise
		_           -> id

testGraph = [((0, 0), [South, East]), ((1, 0), [West, South, East]), ((2, 0), [West]), ((0, 1), [North, East]), ((1, 1), [East, North, West]), ((2, 1), [West])]

-- TODO: check if the performance is acceptable after several rotations
-- if it isn't, one possible solution is to implement a separate way to
-- garbage-collect edges (as distinct from extending their life) and
-- garbage-collect the n'th oldest edges produced by rotation
main = do
	initGUI
	window  <- windowNew
	grid    <- evalStateT (randomGrid 11 11) def
	gridRef <- newIORef grid
	ioStateT (time >>= signal (5, 5) . (+2)) gridRef
	da      <- viewportNew def {
		stabilizationTime = liftM (maybe Already ExactTime . stable) (readIORef gridRef),
		draw     = readIORef gridRef >>= update,
		click    = clickGrid gridRef,
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
	ioStateT (replicateM_ 160 (rotate clockwise (1, 0))) gridRef
	readIORef gridRef >>= putStrLn . take 1 . reverse . show . graph
-}
