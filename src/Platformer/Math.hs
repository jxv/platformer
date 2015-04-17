module Platformer.Math where

import Platformer.Imports

recipNoInf :: (Fractional a, Epsilon a) => a -> a 
recipNoInf x = if nearZero x then 0 else (1 / x)
