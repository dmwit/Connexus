import Direction
import Grid (stable, unsafeStaticGrid, update)
import Viewport

import Data.Default
import Graphics.UI.Gtk hiding (Viewport, viewportNew)

main = do
	initGUI
	window <- windowNew
	grid   <- unsafeStaticGrid [((0, 0), [South, East]), ((1, 0), [West, South, East]), ((2, 0), [West]), ((0, 1), [North, East]), ((1, 1), [East, North, West]), ((2, 1), [West])]
	da     <- viewportNew def {
		stabilizationTime = return . maybe Already ExactTime . stable $ grid,
		draw     = update grid,
		position = Position {
			centerX = def { dimension = 1 }, centerY = def { dimension = 0.5 },
			width   = def { dimension = 3 }, height  = def { dimension = 2   }
			}
		}
	set window [containerChild := da]
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI
