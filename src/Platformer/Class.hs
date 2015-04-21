{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Platformer.Class where

import qualified Data.Map as M
import qualified Data.List as L
import Platformer.Imports
import Platformer.Types

class ToAabb a where
    toAabb :: a -> Aabb

class ToShape a where
    toShape :: a -> Shape


instance Functor Body where
    fmap f a@Body{..} = a{ _bShape = f _bShape }

instance HasRect (Body Rect) where
    rect = bShape

instance HasCircle (Body Circle) where
    circle = bShape

instance ToShape Shape where
    toShape = id

instance ToShape Rect where
    toShape = ShapeRect

instance ToShape Circle where
    toShape = ShapeCircle

instance ToAabb (Body Rect) where
    toAabb a = let
        (V2 px py) = a^.bPosition
        (V2 rx ry) = a^.rRadii
        in Aabb (V2 (px - rx) (py - ry)) (V2 (px + rx) (py + ry))

instance ToAabb (Body Circle) where
    toAabb a = let
        (V2 px py) = a^.bPosition
        r = a^.cRadius
        in Aabb (V2 (px - r) (py - r)) (V2 (px + r) (py + r))
