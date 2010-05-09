-- boilerplate {{{
{-# LANGUAGE NoMonomorphismRestriction #-}
module Viewport (
	Stabilization(..),
	Position(..),
	Dimension(..),
	AnimationState(..),
	Viewport(..),
	viewportNew
) where

import Misc

import Control.Monad
import Data.Default
import Data.IORef
import Data.Maybe
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk hiding (Viewport, viewportNew)
import Graphics.UI.Gtk.Gdk.EventM
-- }}}
-- types {{{
data PointerLocation = PointerLocation { locationTime :: TimeStamp, pos :: (Double, Double) } deriving (Eq, Ord, Show, Read)
data Drag
	= Press  {           current :: PointerLocation }
	| Motion { previous, current :: PointerLocation }
	deriving (Eq, Ord, Show, Read)
data Stabilization   = Already | Never | ExactTime Double deriving (Eq, Ord, Show, Read)
data Position  = Position  { centerX, centerY, width, height :: Dimension }     deriving (Eq, Ord, Show, Read)
data Dimension = Dimension { dimension :: Double, animation :: AnimationState } deriving (Eq, Ord, Show, Read)
data AnimationState
	= Stationary
	| Thrown { startTime, duration, startVelocity :: Double }
	| Goal   { startTime, duration, distance      :: Double }
	deriving (Eq, Ord, Show, Read)

data Viewport = Viewport {
	stabilizationTime :: IO Stabilization,
	draw              :: IO (Render ()),
	click             :: MouseButton -> Double -> Double -> IO (),
	position          :: Position,
	delay             :: Int
	}
-- instances {{{
instance Default a => Default (Render a) where def = return def
instance Default Stabilization  where def = Never
instance Default Position       where def = Position def def def { dimension = 1 } def { dimension = 1 }
instance Default Dimension      where def = Dimension def def
instance Default AnimationState where def = Stationary
instance Default Viewport       where def = Viewport def def def def 15 -- ~60 fps
-- }}}
-- }}}
-- animation {{{
onCenterX, onCenterY, onWidth, onHeight :: (Dimension -> Dimension) -> (Position -> Position)
onCenterX f p = p { centerX = f (centerX p) }
onCenterY f p = p { centerY = f (centerY p) }
onWidth   f p = p { width   = f (width   p) }
onHeight  f p = p { height  = f (height  p) }

onZoomRef, onXYRef :: IORef Position -> (Dimension -> Dimension) -> IO ()
onZoomRef posRef = modifyIORef posRef . liftM2 (.) onWidth   onHeight
onXYRef   posRef = modifyIORef posRef . liftM2 (.) onCenterX onCenterY

freezeAnimation :: Double -> AnimationState -> Double
freezeAnimation _ Stationary = 0
freezeAnimation t' as = freeze' as where
	d = duration as
	s = startTime as
	t = min (t' - s) d -- TODO: also have a case for when the given time is before the animation started
	interpolate t = t + pi / d * sin (t * d / pi)
	freeze' (Thrown { startVelocity = v }) = v / 2 * interpolate t
	freeze' (Goal   { distance      = m }) = m * interpolate t / interpolate d

freeze :: Double -> Dimension -> Double
freeze t d = dimension d + freezeAnimation t (animation d)

final :: Dimension -> Double
final (Dimension { dimension = d, animation = Stationary }) = d
final d = freeze (startTime a + duration a) d where a = animation d

multiply :: Double -> Double -> Double -> Dimension -> Dimension
multiply dur m now dim = Dimension {
	dimension = d,
	animation = Goal {
		startTime = now,
		duration  = dur,
		distance  = m * final dim - d
		}
	} where
	d = freeze now dim

pause :: Double -> Dimension -> Dimension
pause now dim = Dimension {
	dimension = freeze now dim,
	animation = Stationary
	}
-- }}}
-- event handling {{{
-- timeouts {{{
stableTimeout :: DrawingArea -> IORef Stabilization -> IO Bool
stableTimeout da stableRef = do
	widgetQueueDraw da
	stableOld <- readIORef stableRef
	now       <- time
	case stableOld of
		Never       -> return True
		Already     -> return False
		ExactTime t -> do
			let more = now < t
			unless more (writeIORef stableRef Already)
			return more

setStableTime :: DrawingArea -> Int -> IORef Stabilization -> Stabilization -> IO ()
setStableTime da delay stableRef stable = do
	stableOld <- readIORef stableRef
	writeIORef stableRef stable
	when_ (stableOld == Already && stable /= Already)
	      (timeoutAdd (stableTimeout da stableRef) delay)
-- }}}
-- expose {{{
exposeViewport :: IORef Position -> IO (Render a) -> EventM b Bool
exposeViewport posRef draw = do
	dw <- eventWindow
	liftIO $ do
		Position { centerX = cx', centerY = cy', width = pw', height = ph' } <- readIORef posRef
		(dww', dwh') <- drawableGetSize dw
		drawing      <- draw
		now          <- time
		let
			[cx, cy, pw, ph] = map (freeze now) [cx', cy', pw', ph']
			[dww, dwh]       = map (fromIntegral . max 1) [dww', dwh']
			length = min (dww / pw) (dwh / ph)
			tlx    = dww / 2 - cx * length
			tly    = dwh / 2 - cy * length

		renderWithDrawable dw $ do
			translate tlx tly
			scale length length
			drawing
	return True
-- }}}
-- zooming and panning {{{
zoomViewport :: DrawingArea -> IORef Position -> Int -> EventM EScroll Bool
zoomViewport da posRef delay = tryEvent $ do
	sd <- eventScrollDirection
	liftIO $ do
		now       <- time
		stableRef <- newIORef (ExactTime (now + dur))
		zoom sd now
		timeoutAdd (stableTimeout da stableRef) delay
	return ()
	where
	dur = 0.2
	zoom = (onZoomRef posRef .) . multiply dur . zoomFactor
	zoomFactor ScrollUp   = 0.5
	zoomFactor ScrollDown = 2

-- pointerLocation :: (HasTime a, HasCoordinates a) => EventM a PointerLocation
pointerLocation = liftM2 PointerLocation eventTime eventCoordinates

-- TODO: unify with exposeViewport
worldFromScreen :: Double -> IORef Position -> (Double, Double) -> EventM a (Double, Double)
worldFromScreen now posRef (x, y) = do
	dw <- eventWindow
	liftIO $ do
		Position { centerX = cx', centerY = cy', width = pw', height = ph' } <- readIORef posRef
		(dww', dwh') <- drawableGetSize dw
		let
			[cx, cy, pw, ph] = map (freeze now) [cx', cy', pw', ph']
			[dww, dwh]       = map (fromIntegral . max 1) [dww', dwh']
			length = min (dww / pw) (dwh / ph)
			worldX = cx + (x - dww / 2) / length
			worldY = cy + (y - dwh / 2) / length
		return (worldX, worldY)

clickViewport   ::                IORef Drag -> IORef Position                                    -> EventM EButton Bool
dragViewport    :: DrawingArea -> IORef Drag -> IORef Position                                    -> EventM EMotion Bool
releaseViewport :: DrawingArea -> IORef Drag -> IORef Position -> IORef Stabilization -> Viewport -> EventM EButton Bool

clickViewport panRef posRef = tryEvent $ do
	LeftButton <- eventButton
	loc        <- pointerLocation
	liftIO $ do
		now <- time
		pos <- readIORef posRef
		writeIORef panRef (Press loc)
		onXYRef    posRef (pause now)

dragViewport da panRef posRef = do
	old <- liftIO . liftM current . readIORef $ panRef
	new <- pointerLocation
	liftIO $ writeIORef panRef (Motion old new)
	reposition da posRef (pos old) (pos new)
	return True

releaseViewport da panRef posRef stableRef v = do
	b   <- eventButton
	ts  <- eventTime
	dw  <- eventWindow
	now <- time
	pan <- liftIO $ readIORef panRef
	case (b, pan) of
		(LeftButton, Motion {}) -> when (ts == locationTime (current pan)) . liftIO $ do
			-- TODO: unify with exposeViewport / abstract out
			Position { width = pw', height = ph' } <- readIORef posRef
			(dww', dwh') <- drawableGetSize dw
			let
				[pw , ph ] = map (freeze now) [pw', ph']
				[dww, dwh] = map (fromIntegral . max 1) [dww', dwh']
				length = min (dww / pw) (dwh / ph)
				d g = g (current pan) - g (previous pan)
				dx  = d (fst . pos) / length
				dy  = d (snd . pos) / length
				dt  = fromIntegral (d locationTime) / 1000
			modifyIORef posRef . onCenterX $ throw now (-dx / dt)
			modifyIORef posRef . onCenterY $ throw now (-dy / dt)
			-- make sure we're firing off update requests
			stable <- newIORef Already
			setStableTime da (delay v) stable (ExactTime (now + dur))
		_ -> do
			eventCoordinates >>= worldFromScreen now posRef >>= liftIO . uncurry (click v b)
			liftIO $ stabilizationTime v >>= setStableTime da (delay v) stableRef
	return True
	where
	dur = 0.6
	throw now sv dim = Dimension {
		dimension = freeze now dim,
		animation = Thrown { startTime = now, duration = dur, startVelocity = sv }
		}

reposition da posRef old new = do
	now          <- time
	(oldX, oldY) <- worldFromScreen now posRef old
	(newX, newY) <- worldFromScreen now posRef new
	liftIO $ do
		pos <- readIORef posRef
		writeIORef posRef pos {
			centerX = Dimension { dimension = (dimension . centerX) pos + oldX - newX, animation = Stationary },
			centerY = Dimension { dimension = (dimension . centerY) pos + oldY - newY, animation = Stationary }
			}
		widgetQueueDraw da
-- }}}
-- }}}
-- TODO: standardize on GTK's time system: TimeStamp
-- TODO: fix markup in documentation of Graphics.UI.Gtk.Abstract.Widget.motionNotifyEvent
--       figure out what "This may be @Nothing@" means in documentation of G.U.G.A.W.widgetGetDrawWindow
--       fix typo in GUGAW.widgetDelEvents ("disconected" -> "disconnected")
viewportNew :: Viewport -> IO DrawingArea
viewportNew v = do
	da        <- drawingAreaNew
	posRef    <- newIORef (position v)
	panRef    <- newIORef (error "The impossible happened: a drag or button release happened before a button was pressed!")
	stableRef <- newIORef Already
	stabilizationTime v >>= setStableTime da (delay v) stableRef
	widgetAddEvents da [Button1MotionMask]
	on da exposeEvent        $ exposeViewport     posRef (draw  v)
	on da scrollEvent        $ zoomViewport    da posRef (delay v)
	on da buttonPressEvent   $ clickViewport      panRef posRef
	on da motionNotifyEvent  $ dragViewport    da panRef posRef
	on da buttonReleaseEvent $ releaseViewport da panRef posRef stableRef v
	return da
