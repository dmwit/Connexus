-- boilerplate {{{1
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction, TypeFamilies #-}
module Viewport (
	Stabilization(..),
	Position(..),
	Dimension(..),
	AnimationState(..),
	Viewport(..),
	fromStableTime,
	viewportNew
) where

import Misc
import Bounds
import qualified Rectangle as Connexus
import Rounding

import Control.Monad
import Control.Monad.Trans
import Data.Default
import Data.IORef
import Graphics.Rendering.Cairo hiding (width, height)
import Graphics.UI.Gtk hiding (Rectangle(..), Region(..), Screen(..), Viewport, viewportNew)
import qualified Graphics.UI.Gtk as Gtk (Rectangle(..), Region(..))

-- types {{{1
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

type RegionRenderer = Connexus.Region Double -> Render ()
data Viewport = Viewport {
	stabilizationTime :: IO Stabilization,
	draw              :: RegionRenderer,
	click             :: MouseButton -> Double -> Double -> IO (),
	position          :: Position,
	delay             :: Int
	}
-- instances {{{2
instance Default a => Default (Render a) where def = return def
instance Default Stabilization  where def = Never
instance Default Position       where def = Position def def def { dimension = 1 } def { dimension = 1 }
instance Default Dimension      where def = Dimension def def
instance Default AnimationState where def = Stationary
instance Default Viewport       where def = Viewport def def def def 25 -- ~40 fps

-- animation {{{1
-- pure computations {{{2
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
	t = clip 0 d (t' - s)
	clip m n = max m . min n
	interpolate t = t + d / pi * sin (t * pi / d)
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

-- converting between screen and world coordinates {{{2
type Coord = (Double, Double)
data Conversion = Conversion {
	worldFromScreen_   :: Coord -> Coord,
	screenFromWorld_   :: Coord -> Coord,
	pixelsPerWorldUnit :: Double
	}

-- Convertible class {{{3
type family World screen
type family Screen world

type instance World Double = Double
type instance World (a,b) = (World a, World b) -- dang we're lucky
type instance World Gtk.Rectangle = Connexus.Rectangle Double
type instance World [a] = [World a]

type instance Screen Double = Double
type instance Screen (a,b) = (Screen a, Screen b) -- so lucky
type instance Screen (Connexus.Rectangle Double) = Gtk.Rectangle
type instance Screen [a] = [Screen a]

class (World screen ~ world, Screen world ~ screen) => Convertible world screen where
	worldFromScreen :: Conversion -> screen -> world
	screenFromWorld :: Conversion -> world -> screen

instance Convertible Double Double where
	worldFromScreen con len = len / pixelsPerWorldUnit con
	screenFromWorld con len = len * pixelsPerWorldUnit con

instance (a ~ Double, b ~ Double, c ~ Double, d ~ Double) => Convertible (a,b) (c,d) where
	worldFromScreen = worldFromScreen_
	screenFromWorld = screenFromWorld_

instance a ~ Double => Convertible (Connexus.Rectangle a) Gtk.Rectangle where
	worldFromScreen con (Gtk.Rectangle x y w h) = Connexus.Rectangle x' y' w' h' where
		(x', y') = worldFromScreen con (fi x, fi y)
		[w', h'] = map (worldFromScreen con . fi) [w, h]
	-- make a bounding box that's *bigger* than the requested one, if necessary
	screenFromWorld con (Connexus.Rectangle x y w h) = Gtk.Rectangle x' y' w' h' where
		(x'_, y'_) = screenFromWorld con (x, y)
		[x' , y' ] = zipWith (whenPositive floor ceiling) [w, h] [x'_, y'_]
		[w' , h' ] = map (awayFromZero . screenFromWorld con) [x+w - fi x', y+h - fi y']

		whenPositive f g p = if p >= 0 then f else g

instance Convertible a b => Convertible [a] [b] where
	worldFromScreen = map . worldFromScreen
	screenFromWorld = map . screenFromWorld
-- }}}

conversionPure :: (Int, Int) -> Double -> Position -> Conversion
conversionPure (dww', dwh') now pos = Conversion wfs sfw ppwu where
	[cx, cy, w, h] = map (freeze now) . sequence [centerX, centerY, width, height] $ pos
	[dww, dwh]     = map (fi . max 1) [dww', dwh']
	ppwu           = min (dww / w) (dwh / h)
	both f (x, y)  = (f dww cx x, f dwh cy y)
	wfs            = both wfsSingle
	sfw            = both sfwSingle
	wfsSingle screenLength centerWorld screenCoordinate = centerWorld      + (screenCoordinate - screenLength / 2) / ppwu
	sfwSingle screenLength centerWorld worldCoordinate  = screenLength / 2 + (worldCoordinate  - centerWorld     ) * ppwu

conversionImpure :: MonadIO m => DrawWindow -> Double -> IORef Position -> m Conversion
conversionImpure dw now posRef = liftIO $ conversionPure
	`fmap` drawableGetSize dw
	`ap`   return now
	`ap`   readIORef posRef

conversionAt :: Double -> IORef Position -> EventM a Conversion
conversionAt now posRef = eventWindow >>= \dw -> conversionImpure dw now posRef

conversion :: IORef Position -> EventM a Conversion
conversion posRef = time >>= flip conversionAt posRef

-- event handling {{{1
-- timeouts {{{2
fromStableTime :: NegInf Double -> Stabilization
fromStableTime Infinity   = Already
fromStableTime (Finite t) = ExactTime t

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

-- expose {{{2
exposeViewport :: IORef Position -> RegionRenderer -> EventM EExpose Bool
exposeViewport posRef draw = do
	dw      <- eventWindow
	con     <- conversion posRef
	region  <- eventRegion >>= liftIO . regionGetRectangles
	let
		length     = pixelsPerWorldUnit con
		(tlx, tly) = worldFromScreen con (0, 0)
	liftIO . renderWithDrawable dw $ do
		scale length length
		translate (-tlx) (-tly)
		draw $ worldFromScreen con region
	return True

-- zooming {{{2
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
	zoomFactor _ = 1

-- panning {{{2
-- pointerLocation :: (HasTime a, HasCoordinates a) => EventM a PointerLocation
pointerLocation = liftM2 PointerLocation eventTime eventCoordinates

clickViewport   ::                IORef Drag -> IORef Position                                    -> EventM EButton Bool
dragViewport    :: DrawingArea -> IORef Drag -> IORef Position                                    -> EventM EMotion Bool
releaseViewport :: DrawingArea -> IORef Drag -> IORef Position -> IORef Stabilization -> Viewport -> EventM EButton Bool

clickViewport panRef posRef = tryEvent $ do
	MiddleButton <- eventButton
	loc          <- pointerLocation
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
	now <- time
	pan <- liftIO $ readIORef panRef
	con <- conversionAt now posRef
	case (b, pan) of
		(MiddleButton, Motion {}) -> when (ts == locationTime (current pan)) . liftIO $ do
			let
				d :: Num a => (PointerLocation -> a) -> a
				d g = g (current pan) - g (previous pan)
				dx  = worldFromScreen con $ d (fst . pos)
				dy  = worldFromScreen con $ d (snd . pos)
				dt  = fi (d locationTime) / 1000
			modifyIORef posRef . onCenterX $ throw now (-dx / dt)
			modifyIORef posRef . onCenterY $ throw now (-dy / dt)
			-- make sure we're firing off update requests
			stable <- newIORef Already
			setStableTime da (delay v) stable (ExactTime (now + dur))
		_ -> do
			eventCoordinates >>= liftIO . uncurry (click v b) . worldFromScreen con
			liftIO $ widgetQueueDraw da
			liftIO $ stabilizationTime v >>= setStableTime da (delay v) stableRef
	return True
	where
	dur = 0.6
	throw now sv dim = Dimension {
		dimension = freeze now dim,
		animation = Thrown { startTime = now, duration = dur, startVelocity = sv }
		}

reposition da posRef old new = do
	con <- conversion posRef
	let
		(oldX, oldY) = worldFromScreen con old
		(newX, newY) = worldFromScreen con new
	liftIO . modifyIORef posRef $ \pos -> pos {
		centerX = Dimension { dimension = (dimension . centerX) pos + oldX - newX, animation = Stationary },
		centerY = Dimension { dimension = (dimension . centerY) pos + oldY - newY, animation = Stationary }
		}
	liftIO $ widgetQueueDraw da

-- viewportNew {{{1
viewportNew :: Viewport -> IO DrawingArea
viewportNew v = do
	da        <- drawingAreaNew
	posRef    <- newIORef (position v)
	panRef    <- newIORef (error "The impossible happened: a drag or button release happened before a button was pressed!")
	stableRef <- newIORef Already
	stabilizationTime v >>= setStableTime da (delay v) stableRef
	widgetAddEvents da [Button2MotionMask]
	on da exposeEvent        $ exposeViewport     posRef (draw  v)
	on da scrollEvent        $ zoomViewport    da posRef (delay v)
	on da buttonPressEvent   $ clickViewport      panRef posRef
	on da motionNotifyEvent  $ dragViewport    da panRef posRef
	on da buttonReleaseEvent $ releaseViewport da panRef posRef stableRef v
	return da
