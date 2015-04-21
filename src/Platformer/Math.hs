module Platformer.Math where

import Platformer.Imports

recipNoInf :: (Fractional a, Epsilon a) => a -> a 
recipNoInf x = if nearZero x then 0 else (1 / x)
{-# INLINE recipNoInf #-}

sqlen :: V2 Float -> Float
sqlen (V2 x y) = x^2 + y^2
{-# INLINE sqlen #-}

clamp :: (Applicative f, Ord a) => f a -> f a -> f a -> f a
clamp low high n = max <$> low <*> (min <$> high <*> n)
{-# INLINE clamp #-}

