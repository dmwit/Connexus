module Rectangle where

-- TODO: use vector-space here and through the rest of the program
-- axis-aligned, homogeneous, 2D rectangles
data Rectangle a = Rectangle { x, y, w, h :: a } deriving (Eq, Ord, Show, Read)

type Region a = [Rectangle a]
