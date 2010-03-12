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

clockwise North = East
clockwise East  = South
clockwise South = West
clockwise West  = North

aboutFace = clockwise . clockwise
counterclockwise = clockwise . clockwise . clockwise
