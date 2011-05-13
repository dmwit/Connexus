-- boilerplate {{{
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Lock where

import Misc

import Control.Monad.State
import Data.Function
import Data.IORef
import Data.Set (Set)
import Graphics.Rendering.Cairo
import qualified Data.Set as Set
-- }}}
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

-- TODO: do somekind of pretty fuzzy cloud
update lock = do
	setLineWidth 0
	setSourceRGBA 0 0 0 0.2
	mapM_ (uncurry (rect `on` fromIntegral)) (Set.elems lock)
	fill
	where rect x y = rectangle (x - 0.5) (y - 0.5) 1 1
