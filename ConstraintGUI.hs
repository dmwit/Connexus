-- boilerplate {{{1
import Constraint
import Direction
import Grid hiding (width, height)
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

-- code {{{1
main = do
	initGUI
	args    <- getArgs
	let (w, h) = case map reads args of
		[[(w, "")], [(h, "")]] -> (w, h)
		_ -> (11, 11)
	window  <- windowNew
	grid    <- evalStateT (randomGrid w h) def
	solver  <- solverFromGrid (nodeShape grid)
	da      <- viewportNew def {
		stabilizationTime = return Never,
		draw     = step solver >> Constraint.update solver,
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
