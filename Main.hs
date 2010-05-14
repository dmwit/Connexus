import Direction
import Grid (Grid, rotate, stable, unsafeStaticGrid, update)
import Misc
import Viewport

import Control.Monad
import Control.Monad.Reader
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

-- TODO: the performance is miserable after several rotations, even with such a
-- tiny graph! figure out how to fix it; for example, maybe add a separate way
-- to garbage collect edges (as distinct from ending their life) and garbage
-- collect the n-th oldest edges deleted via rotation
main = do
	initGUI
	window  <- windowNew
	grid    <- unsafeStaticGrid testGraph
	gridRef <- newIORef grid
	da      <- viewportNew def {
		-- TODO: there's a bug with stability somewhere; the viewport doesn't
		-- update for long enough
		stabilizationTime = liftM (maybe Already ExactTime . stable) (readIORef gridRef),
		draw     = readIORef gridRef >>= update,
		click    = clickGrid gridRef,
		position = Position {
			centerX = def { dimension = 1 }, centerY = def { dimension = 0.5 },
			width   = def { dimension = 3 }, height  = def { dimension = 2   }
			}
		}
	set window [containerChild := da]
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI
