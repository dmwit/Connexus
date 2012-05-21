-- boilerplate {{{1
import Constraint
import Direction hiding (step)
import Grid
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

main = do
	initGUI
	args    <- getArgs
	let (w, h) = case map reads args of
		[[(w, "")], [(h, "")]] -> (w, h)
		_ -> (11, 11)
	window  <- windowNew
	grid    <- randomGrid w h
	solver  <- solverFromGrid (pieces grid)
	da      <- viewportNew def {
		stabilizationTime = return Never,
		draw     = liftIO (step solver) >> Constraint.update solver,
		delay    = 15,
		position = Position {
			centerX = def { dimension = (fromIntegral w - 1) / 2 }, centerY = def { dimension = (fromIntegral h - 1) / 2 },
			width   = def { dimension =  fromIntegral w + 2      }, height  = def { dimension =  fromIntegral h + 2      }
			}
		}
	set window [containerChild := da]
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI
