module Platformer.Class where

import qualified Data.Map as M
import qualified Data.List as L
import Platformer.Imports
import Platformer.Types

class ToAabb a where
    toAabb :: a -> Aabb

instance ToAabb Body where
    toAabb a = let
        (V2 px py) = a^.bPosition
        (V2 rx ry) = a^.bRadii
        in Aabb (V2 (px - rx) (py - ry)) (V2 (px + rx) (py + ry))
