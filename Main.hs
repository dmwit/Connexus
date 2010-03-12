import Direction
import Grid

import Data.Time
import Graphics.UI.Gtk

arbitraryUTCTime = UTCTime (ModifiedJulianDay 55000) (secondsToDiffTime 0)
time = fmap (toRational . flip diffUTCTime arbitraryUTCTime) getCurrentTime

main = do
	initGUI
	window <- windowNew
	da     <- drawingAreaNew
	grid   <- unsafeStaticGrid [((0, 0), [South, East]), ((1, 0), [West, South, East]), ((2, 0), [West]), ((0, 1), [East]), ((1, 1), [East, North, West]), ((2, 1), [West])]
	set window [containerChild := da]
	on da exposeEvent (update grid)
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI
