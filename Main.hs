import Direction
import Grid

import Graphics.UI.Gtk

invalidate da = do
	dw         <- widgetGetDrawWindow da
	(dww, dwh) <- drawableGetSize dw
	drawWindowInvalidateRect dw (Rectangle 0 0 dww dwh) True

main = do
	initGUI
	window <- windowNew
	da     <- drawingAreaNew
	grid   <- unsafeStaticGrid [((0, 0), [South, East]), ((1, 0), [West, South, East]), ((2, 0), [West]), ((0, 1), [East]), ((1, 1), [East, North, West]), ((2, 1), [West])]
	set window [containerChild := da]
	on da exposeEvent (update grid)
	timeoutAdd (invalidate da >> return True) 15 -- ~60 fps
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI
