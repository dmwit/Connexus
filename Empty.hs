module Empty where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

class    Empty e         where empty :: e
instance Empty ()        where empty = ()
instance Empty [a]       where empty = []
instance Empty (Map k v) where empty = Map.empty
instance Empty (Set e)   where empty = Set.empty
instance (Empty a, Empty b) => Empty (a, b) where empty = (empty, empty)
