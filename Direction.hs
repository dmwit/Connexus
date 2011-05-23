module Direction where

import Data.Ix

data Direction = North | East | South | West deriving (Eq, Ord, Show, Read, Bounded, Enum, Ix)

dx North =  0
dx East  =  1
dx South =  0
dx West  = -1

dy North = -1
dy East  =  0
dy South =  1
dy West  =  0

angle North = 3 * pi / 2
angle East  = 0
angle South = pi / 2
angle West  = pi

step d (x, y) = (x + dx d, y + dy d)

class Oriented a where
	clockwise, aboutFace, counterclockwise :: a -> a
	clockwise = counterclockwise . counterclockwise . counterclockwise
	counterclockwise = clockwise . clockwise . clockwise
	aboutFace = clockwise . clockwise

instance Oriented Direction where
	clockwise North = East
	clockwise East  = South
	clockwise South = West
	clockwise West  = North
