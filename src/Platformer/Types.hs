{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Platformer.Types where

import Platformer.Imports
import Data.Data
import GHC.Generics (Generic1)
import qualified Data.Vector as V
import qualified Data.Array.IO as A

data BodyType
    = Static
    | Dynamic
    deriving (Show, Eq)

data Shape
    = ShapeRect !Rect
    | ShapeCircle !Circle
    deriving (Show, Eq)

data Aabb = Aabb {
    _aMin :: !(V2 Float),
    _aMax :: !(V2 Float)
} deriving (Show, Eq)

data Rect = Rect {
    _rRadii :: !(V2 Float)
} deriving (Show, Eq)

data Circle = Circle {
    _cRadius :: !Float
} deriving (Show, Eq)

data Body a = Body {
    _bType :: !BodyType,
    _bShape :: !a,
    _bPosition :: !(V2 Float),
    _bVelocity :: !(V2 Float),
    _bForce :: !(V2 Float),
    _bMass :: !Float,
    _bInverseMass :: !Float,
    _bStaticFriction :: !Float,
    _bDynamicFriction :: !Float,
    _bRestitution :: !Float
} deriving (Show, Eq, Typeable, Generic1)

data Manifold = Manifold {
    _mfNormal :: !(V2 Float),
    _mfPenetration :: !Float,
    _mfAKey :: !Int, 
    _mfBKey :: !Int,
    _mfE :: !Float,
    _mfDynamicFriction :: !Float,
    _mfStaticFriction :: !Float
} deriving (Show, Eq)

data World = World {
    _wBroadphase :: [(Int, Body Shape)] -> [[(Int, Body Shape)]],
    _wBodies :: !(A.IOArray Int (Body Shape)),
    _wManifolds :: ![Manifold],
    _wUsedBodyKeys :: !(Set Int),
    _wUnusedBodyKeys :: ![Int],
    _wDeltaTime :: !Float,
    _wGravity :: !(V2 Float),
    _wIterations :: !Int
} deriving (Show)

type Broadphase = [(Int, Body Shape)] -> [[(Int, Body Shape)]]

instance Show (A.IOArray Int (Body Shape)) where
    show _ = "Bodies"

instance Show Broadphase where
    show _ = "Broadphase"

instance Show (MVector RealWorld (Body Shape)) where
    show _ = "Bodies"

makeClassy ''Aabb
makeClassy ''Rect
makeClassy ''Circle
makeClassy ''Body
makeClassy ''Manifold
makeClassy ''World
