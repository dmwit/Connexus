-- boilerplate {{{1
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Lock where

import Direction
import Misc

import Control.Monad.State
import Data.Function
import Data.IORef
import Data.Set (Set)
import Graphics.Rendering.Cairo
import qualified Data.Set as Set

-- code {{{1
type Lock a = Set a

cond         :: (Ord a, MonadState (Lock a) m) => a -> m b -> m b -> m b
whenUnlocked :: (Ord a, MonadState (Lock a) m) => a -> m () -> m ()
toggle       :: (Ord a, MonadState (Lock a) m) => a -> m ()
update       :: Lock Point -> Render ()

cond a t f = do
	on <- gets (a `Set.member`)
	if on then t else f

whenUnlocked = flip cond (return ())
toggle a = cond a (modify (Set.delete a)) (modify (Set.insert a))

update lock = do
	setLineWidth 0
	setSourceRGBA 0 0 0 0.2
	forM_ (Set.elems lock) $ \pos@(x', y') -> do
		let (x, y) = (fromIntegral x', fromIntegral y')
		moveTo (x + pclock dx North) (y + pclock dy North)
		forM_ [North, East, South, West] $ \d ->
			corner x y d
				(step d pos `Set.member` lock)
				(step (clockwise d) (step d pos) `Set.member` lock)
				(step (clockwise d) pos `Set.member` lock)
	fill

-- Given a direction d, trace the path appropriate for the d-(clockwise d)
-- corner. (For example, if d is West, we will trace
-- the northwest corner.) The three remaining arguments tell whether particular neighbors are locked:
--   * one step in the d direction
--   * one step in the d direction and one step in the (clockwise d) direction
--   * one step in the (clockwise d) direction
corner x y d nd ndrd nrd
	| not (nd || ndrd || nrd) =
		arc
			(x + full dx d' d / 2)
			(y + full dy d' d / 2)
			0.25 (angle d) (angle d')
	| ndrd && not (nd || nrd) =
		arcNegative
			(x + dx d + full dx d' d'' / 2)
			(y + dy d + full dy d' d'' / 2)
			0.25 (angle d'') (angle d'' - pi / 2)
	| nd && ndrd && not nrd =
		arcNegative
			(x + dx d' + full dx d''' d / 2)
			(y + dy d' + full dy d''' d / 2)
			0.25 (angle d''' + pi / 2) (angle d''')
	| otherwise =
		lineTo (x + full dx d' d) (y + full dy d' d)
	where
	(_:d':d'':d''':_) = iterate clockwise d

full    f d1 d2 = f d1 / 2 + f d2 / 2
partial f d1 d2 = f d1 / 2 + f d2 / 4
pclock  f d     = partial f d (clockwise d)
