module Platformer.Util where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.QuadTree as QT
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.ST.Strict
import Control.Monad.Primitive
import Platformer.Imports
import Platformer.Types
import Platformer.Class
import Platformer.Math
import Safe

import Debug.Trace (traceShowId)

isIntersect :: Aabb -> Aabb -> Bool
isIntersect (Aabb (V2 al au) (V2 ar ad)) (Aabb (V2 bl bu) (V2 br bd)) = ar >= bl && al <= br && ad >= bu && au <= bd
{-# INLINE isIntersect #-}

isInside :: V2 Float -> Aabb -> Bool
isInside (V2 x y) = isIntersect (Aabb (V2 x y) (V2 x y))
{-# INLINE isInside #-}

testBody :: Aabb -> Body Shape -> Bool
testBody a b@Body{_bShape = ShapeRect rb} = testRect a (rb <$ b)
testBody a b@Body{_bShape = ShapeCircle cb} = testCircle a (cb <$ b)

testRect :: Aabb -> Body Rect -> Bool
testRect a b = isIntersect a (toAabb b)
{-# INLINE testRect #-}

testCircle :: Aabb -> Body Circle -> Bool
testCircle a b = let
    inside = isInside (b^.bPosition) a
    closest = clamp (a^.aMin) (a^.aMax) (b^.bPosition)
    posDiff = b^.bPosition - (a^.aMin + a^.aMax) / 2
    normal = posDiff - closest
    distSq = sqlen normal
    in inside || distSq <= (b^.cRadius) ^ 2
{-# INLINE testCircle #-}

bodyToBody :: Body Shape -> Body Shape -> Maybe (V2 Float, Float)
bodyToBody a@Body{_bShape = ShapeRect ra} b@Body{_bShape = ShapeRect rb} = rectToRect (ra <$ a) (rb <$ b)
bodyToBody a@Body{_bShape = ShapeRect ra} b@Body{_bShape = ShapeCircle cb} = rectToCircle (ra <$ a) (cb <$ b)
bodyToBody a@Body{_bShape = ShapeCircle ca} b@Body{_bShape = ShapeCircle cb} = circleToCircle (ca <$ a) (cb <$ b)
bodyToBody a b = (_1 %~ negate) <$> bodyToBody b a

rectToRect :: Body Rect -> Body Rect -> Maybe (V2 Float, Float)
rectToRect a b = let
    n = b^.bPosition - a^.bPosition
    overlap = a^.rRadii + b^.rRadii - abs n
    in if not $ isIntersect (toAabb a) (toAabb b)
        then Nothing
        else Just $
            if abs (overlap^._x) < abs (overlap^._y)
            then (if n^._x < 0 then V2 (-1) 0 else V2 1 0, overlap^._x)
            else (if n^._y < 0 then V2 0 (-1) else V2 0 1, overlap^._y)
{-# INLINE rectToRect #-}

rectToCircle :: Body Rect -> Body Circle -> Maybe (V2 Float, Float)
rectToCircle a b = let
    outL = b^.bPosition^._x < a^.bPosition^._x - a^.rRadii^._x
    outR = b^.bPosition^._x > a^.bPosition^._x + a^.rRadii^._x
    outU = b^.bPosition^._y < a^.bPosition^._y - a^.rRadii^._y
    outD = b^.bPosition^._y > a^.bPosition^._x + a^.rRadii^._y
    in if (outL || outR) && (outU || outD)
        -- Circle may hit a rect's corner.
        -- Treat as circle/point-to-circle collision.
        then let
            toCorner x y = bPosition %~ (_x `x` (a^.rRadii^._x)) . (_y `y` (a^.rRadii^._y))
            cornered = a & toCorner (if outL then (-~) else (+~)) (if outU then (-~) else (+~))
            in circleToCircle (Circle 0 <$ cornered) b
        -- Circle may hit a rect's face.
        -- Treat as rect-to-rect collision.
        else rectToRect a $ Rect (pure $ b^.cRadius) <$ b
{-# INLINE rectToCircle #-}

circleToCircle :: Body Circle -> Body Circle -> Maybe (V2 Float, Float)
circleToCircle a b = let
    normal = b^.bPosition - a^.bPosition
    distSq = sqlen normal
    dist = sqrt distSq
    radius = a^.cRadius + b^.cRadius
    in if distSq >= radius * radius
        then Nothing
        else Just $
            if dist == 0
            then (V2 1 0, a^.cRadius)
            else (normal ^/ dist, radius - dist)
{-# INLINE circleToCircle #-}

testBodyToBody :: Body Shape -> Body Shape -> Bool
testBodyToBody a@Body{_bShape = ShapeRect ra} b@Body{_bShape = ShapeRect rb} = testRectToRect (ra <$ a) (rb <$ b)
testBodyToBody a@Body{_bShape = ShapeRect ra} b@Body{_bShape = ShapeCircle cb} = testRectToCircle (ra <$ a) (cb <$ b)
testBodyToBody a@Body{_bShape = ShapeCircle ca} b@Body{_bShape = ShapeCircle cb} = testCircleToCircle (ca <$ a) (cb <$ b)
testBodyToBody a b = testBodyToBody b a

testRectToRect :: Body Rect -> Body Rect -> Bool
testRectToRect a b = isIntersect (toAabb a) (toAabb b)
{-# INLINE testRectToRect #-}

testRectToCircle :: Body Rect -> Body Circle -> Bool
testRectToCircle a b = let
    outL = b^.bPosition^._x < a^.bPosition^._x - a^.rRadii^._x
    outR = b^.bPosition^._x > a^.bPosition^._x + a^.rRadii^._x
    outU = b^.bPosition^._y < a^.bPosition^._y - a^.rRadii^._y
    outD = b^.bPosition^._y > a^.bPosition^._x + a^.rRadii^._y
    in if (outL || outR) && (outU || outD)
        then let
            toCorner x y = bPosition %~ (_x `x` (a^.rRadii^._x)) . (_y `y` (a^.rRadii^._y))
            cornered = a & toCorner (if outL then (-~) else (+~)) (if outU then (-~) else (+~))
            in testCircleToCircle (Circle 0 <$ cornered) b
        else testRectToRect a $ Rect (pure $ b^.cRadius) <$ b
{-# INLINE testRectToCircle #-}

testCircleToCircle :: Body Circle -> Body Circle -> Bool
testCircleToCircle a b = let
    normal = b^.bPosition - a^.bPosition
    distSq = sqlen normal
    radius = a^.cRadius + b^.cRadius
    in distSq < radius * radius
{-# INLINE testCircleToCircle #-}

solveCollision :: (Int, Body Shape) -> (Int, Body Shape) -> Maybe Manifold
solveCollision (akey,a) (bkey,b) = do
    (normal, penetration) <- bodyToBody a b
    return $ Manifold {
        _mfNormal = normal,
        _mfPenetration = penetration,
        _mfAKey = akey,
        _mfBKey = bkey,
        _mfE = a^.bRestitution * b^.bRestitution,
        _mfDynamicFriction = a^.bDynamicFriction * b^.bDynamicFriction,
        _mfStaticFriction = a^.bStaticFriction * b^.bStaticFriction
    }

integrateForce :: Float -> V2 Float -> Body Shape -> Body Shape
integrateForce dt gravity b@Body{..} = let
    vel = (_bForce ^* _bInverseMass + gravity) ^* (dt / 2)
    in b & if _bInverseMass == 0 then id else bVelocity +~ vel

integrateVelocity :: Float -> V2 Float -> Body Shape -> Body Shape
integrateVelocity dt gravity b@Body{..} = let
    pos = _bVelocity ^* dt
    b' = b & bPosition +~ pos
    in if _bInverseMass == 0 then b else integrateForce dt gravity b'

stepper :: World -> World
stepper
    = clearForces
    . clearManifolds
    . correctPositions
    . integrateVelocities
    . solveCollisions
    . initializeCollisions
    . integrateForces
    . genCollisionInfo

genCollisionInfo :: World -> World
genCollisionInfo w@World{..} = w & wManifolds .~ mfs
 where
    mfs = catMaybes
        [ solveCollision apair bpair
        | bucket <- _wBroadphase (M.toList _wBodies)
        , (apair@(akey, a), bodies) <- zip bucket (tail (L.tails bucket))
        , let aim = a^.bInverseMass /= 0
        , let av = nearZero (a^.bVelocity)
        , bpair@(bkey, b) <- bodies
        , aim || b^.bInverseMass /= 0
        , not $ av && nearZero (b^.bVelocity) -- Don't solve for both sleeping objects
        ]

integrateForces :: World -> World
integrateForces w@World{..} = w & wBodies %~ (M.map (integrateForce _wDeltaTime _wGravity))

initializeCollisions :: World -> World
initializeCollisions w@World{..} = w & wManifolds %~ (map step)
 where
    step m@Manifold{..} = manifoldInitialize _wGravity _wDeltaTime _mfAKey (_wBodies M.! _mfAKey) _mfBKey (_wBodies M.! _mfBKey) m
    manifoldInitialize g dt akey a bkey b m = let
        e = mfE .~ (a^.bRestitution * b^.bRestitution)
        sf = mfStaticFriction .~ (a^.bStaticFriction * b^.bStaticFriction)
        df = mfDynamicFriction .~ (a^.bDynamicFriction * b^.bDynamicFriction)
        in m & e . sf . df

solveCollisions :: World -> World
solveCollisions w = (iterate stepper w) !! (w^.wIterations)
 where
    stepper v = v & wBodies .~ (foldl step (v^.wBodies) (v^.wManifolds))
    step bodies m = let
        (akey,bkey) = (m^.mfAKey, m^.mfBKey)
        (a,b) = manifoldApplyImpulse (bodies M.! akey) (bodies M.! bkey) m
        in M.insert akey a $ M.insert bkey b $ bodies

integrateVelocities :: World -> World
integrateVelocities w@World{..} = w & wBodies %~ M.map (integrateVelocity _wDeltaTime _wGravity)

correctPositions :: World -> World
correctPositions w = w & wBodies .~ (foldl step (w^.wBodies) (w^.wManifolds))
 where
    step bodies m = let
        (akey,bkey) = (m^.mfAKey, m^.mfBKey)
        (a,b) = positionalCorrection (bodies M.! akey) (bodies M.! bkey) m
        in M.insert akey a $ M.insert bkey b $ bodies
    positionalCorrection a b Manifold{..} = let
        percent = 0.4 -- usually 0.2 to 0.8
        slop = 0.05 -- usually 0.01 to 0.1 (to stop jitters)
        correction = ((max 0 (_mfPenetration - slop)) / (a^.inverseMass + b^.inverseMass)) * percent *^ _mfNormal
        a' = a & bPosition -~ (a^.inverseMass *^ correction)
        b' = b & bPosition +~ (b^.inverseMass *^ correction)
        in (a',b')

onDynamic :: Body Shape -> (Body Shape -> Body Shape) -> Body Shape
onDynamic a f = (if a^.bType == Dynamic then f else id) a

clearForces :: World -> World
clearForces = wBodies %~ M.map (bForce .~ zero)

clearManifolds :: World -> World
clearManifolds = wManifolds .~ []

addBody :: Body Shape -> World -> (Int, World)
addBody a w = let
    key = head (w^.wUnusedBodyKeys)
    w' = w & (wBodies %~ M.insert key a) . (wUnusedBodyKeys %~ tail)
    in (key, w')

addBody' :: Body Shape -> World -> World
addBody' a w = let
    key = head (w^.wUnusedBodyKeys)
    in w & (wBodies %~ M.insert key a) . (wUnusedBodyKeys %~ tail)

addBodyMay :: Body Shape -> World -> Maybe (Int, World)
addBodyMay a w = do
    key <- headMay (w^.wUnusedBodyKeys)
    let w' = w & (wBodies %~ M.insert key a) . (wUnusedBodyKeys %~ tail)
    return (key, w')

deleteBody :: Int -> World -> World
deleteBody key = (wBodies %~ M.delete key) . (wUnusedBodyKeys %~ (key:))

manifoldApplyImpulse :: Body Shape -> Body Shape -> Manifold -> (Body Shape, Body Shape)
manifoldApplyImpulse a b m = let
    rv = b^.bVelocity - a^.bVelocity
    contactVel = dot rv (m^.mfNormal)
    e = min (a^.bRestitution) (b^.bRestitution)
    invMassSum = a^.bInverseMass + b^.bInverseMass
    j = ((negate $ 1 + e) * contactVel) / invMassSum
    impulse = j *^ m^.mfNormal
    a' = a & (bVelocity -~ (a^.bInverseMass *^ impulse))
    b' = b & (bVelocity +~ (b^.bInverseMass *^ impulse))
    --
    rv' = b'^.bVelocity - a'^.bVelocity
    t = normalize $ rv' - (m^.mfNormal) ^* dot rv' (m^.mfNormal)
    jt = (negate $ dot rv' t) / invMassSum
    tagImpulse = if abs jt < j * (m^.mfStaticFriction)
        then t ^* jt
        else t ^* ((-j) * (m^.mfDynamicFriction))
    a'' = a' & (bVelocity -~ (a'^.bInverseMass *^ tagImpulse))
    b'' = b' & (bVelocity +~ (b'^.bInverseMass *^ tagImpulse))
    in if 
        | nearZero invMassSum -> (a & bVelocity .~ zero, b & bVelocity .~ zero)
        | contactVel > 0 -> (a,b)
        | nearZero jt -> (a',b')
        | otherwise -> (a'',b'')

newWorld :: Int -> World
newWorld maxBodies = World {
    _wBroadphase = (:[]),
    _wBodies = M.empty,
    _wManifolds = [],
    _wUnusedBodyKeys = [0..maxBodies - 1],
    _wGravity = V2 0 9.8,
    _wDeltaTime = 1 / 60,
    _wIterations = 10
}

newBody :: ToShape a => a -> Body Shape
newBody a = Body {
    _bType = Dynamic,
    _bShape = toShape a,
    _bPosition = zero,
    _bVelocity = zero,
    _bForce = zero,
    _bMass = 1,
    _bInverseMass = 1,
    _bStaticFriction = 0.5,
    _bDynamicFriction = 0.5,
    _bRestitution = 0.5
}

newRect :: V2 Float -> Body Shape
newRect radii = newBody (Rect radii)

newCircle :: Float -> Body Shape
newCircle radius = newBody (Circle radius)

mass :: Lens' (Body a) Float
mass = interrelateInv bMass bInverseMass

inverseMass :: Lens' (Body a) Float
inverseMass = interrelateInv bInverseMass bMass

interrelateInv :: Lens' a Float -> Lens' a Float -> Lens' a Float
interrelateInv a b = lens (^.a) (\x y -> x & (a .~ y) . (b .~ recipNoInf y))

massFromDensity :: Float -> Shape -> Float
massFromDensity density (ShapeRect r) = density * 4 * r^.rRadii^._x * r^.rRadii^._y
massFromDensity density (ShapeCircle c) = density * pi * (c^.cRadius)^2 

-- Pontentially splits bodies in a world
destroyBodiesByArea :: Aabb -> World -> (World, [Body a])
destroyBodiesByArea = undefined

{-
Rock       Density : 0.6  Restitution : 0.1
Wood       Density : 0.3  Restitution : 0.2
Metal      Density : 1.2  Restitution : 0.05
BouncyBall Density : 0.3  Restitution : 0.8
SuperBall  Density : 0.3  Restitution : 0.95
Pillow     Density : 0.1  Restitution : 0.2
Static     Density : 0.0  Restitution : 0.4
-}

type Broadphase = [(Int, Body Shape)] -> [[(Int, Body Shape)]]

uniformGrid :: V2 Int -> Float -> Broadphase
uniformGrid (V2 w h) bucketSide bodyPairs = zipWith testFilter tests (repeat bodyPairs)
 where
    testFilter test = filter (test . snd)
    tests = map testBody aabbs
    aabbs =
        [ Aabb topleft (topleft + pure bucketSide)
        | y <- [0..h - 1]
        , x <- [0..w - 1]
        , let topleft = fmap fromIntegral (V2 x y) ^* bucketSide
        ]

uniformHash :: (Int,Int) -> Float -> Broadphase
uniformHash (w,h) bucketSide bodyPairs = let
    len = w * h
    inserts m = do
        forM_ bodyPairs $ \bpair -> do
            let loc = let
                    (V2 x y) = (snd bpair)^.bPosition ^/ bucketSide
                    in (round y * w + round x) `mod` len
            hashInsert m loc bpair
        return m
    hashInsert m loc bpair = do
        bucket <- VM.unsafeRead m loc
        VM.unsafeWrite m loc (bpair : bucket)
        return m
    v = V.create $ inserts =<< V.thaw (V.fromListN len (repeat []))
    in V.toList v
