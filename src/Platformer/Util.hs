module Platformer.Util where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.QuadTree as QT
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Array.Repa as R
import qualified Data.Array.IO as A
import Data.Foldable (sequenceA_)
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

stepper :: World -> IO World
stepper w
    = genCollisionInfo w
    >>= integrateForces
    >>= initializeCollisions
    >>= solveCollisions
    >>= integrateVelocities
    >>= correctPositions
    >>= clearManifolds
    >>= clearForces

genCollisionInfo :: World -> IO World
genCollisionInfo w@World{..} = do
    bodies <- getUsedBodies w
    return $ w & wManifolds .~ (mfs bodies)
 where
    mfs usedBodies = catMaybes
        [ solveCollision apair bpair
        | bucket <- _wBroadphase usedBodies
        , (apair@(akey, a), bodies) <- zip bucket (tail (L.tails bucket))
        , let aim = a^.bInverseMass /= 0
        , let av = nearZero (a^.bVelocity)
        , bpair@(bkey, b) <- bodies
        , aim || b^.bInverseMass /= 0
        , not $ av && nearZero (b^.bVelocity) -- Don't solve for both sleeping objects
        ]

getUsedBodies :: World -> IO [(Int, Body Shape)]
getUsedBodies w = forM (Set.toList $ w^.wUsedBodyKeys) $ \key -> do
    a <- A.readArray (w^.wBodies) key
    return (key, a)

integrateForces :: World -> IO World
integrateForces w@World{..} = do
    bodies <- A.mapArray (integrateForce _wDeltaTime _wGravity) (w^.wBodies)
    return $ w & wBodies .~ bodies

initializeCollisions :: World -> IO World
initializeCollisions w@World{..} = do
    mfs <- forM _wManifolds step
    return $ w & wManifolds .~ mfs
 where
    step :: Manifold -> IO Manifold
    step m@Manifold{..} = do
        a <- A.readArray _wBodies _mfAKey
        b <- A.readArray _wBodies _mfBKey
        return $ manifoldInitialize _wGravity _wDeltaTime a b m
    manifoldInitialize g dt a b m = let
        e = mfE .~ (a^.bRestitution * b^.bRestitution)
        sf = mfStaticFriction .~ (a^.bStaticFriction * b^.bStaticFriction)
        df = mfDynamicFriction .~ (a^.bDynamicFriction * b^.bDynamicFriction)
        in m & e . sf . df

solveCollisions :: World -> IO World
solveCollisions w@World{..} = do
    sequenceA_ (replicate _wIterations $ forM_ _wManifolds step)
    return w 
 where
    step :: Manifold -> IO ()
    step m@Manifold{..} = do
        a <- A.readArray _wBodies _mfAKey
        b <- A.readArray _wBodies _mfBKey
        let (a',b') = manifoldApplyImpulse a b m
        A.writeArray _wBodies _mfAKey a'
        A.writeArray _wBodies _mfBKey b'

integrateVelocities :: World -> IO World
integrateVelocities w@World{..} = do
    bodies <- A.mapArray (integrateVelocity _wDeltaTime _wGravity) (w^.wBodies)
    return $ w & wBodies .~ bodies

correctPositions :: World -> IO World
correctPositions w@World{..} = do
    forM_ _wManifolds step
    return w
 where
    step :: Manifold -> IO ()
    step m@Manifold{..} = do
        a <- A.readArray _wBodies _mfAKey
        b <- A.readArray _wBodies _mfBKey
        let (a',b') = positionalCorrection a b m
        A.writeArray _wBodies _mfAKey a'
        A.writeArray _wBodies _mfBKey b'
    positionalCorrection :: Body Shape -> Body Shape -> Manifold -> (Body Shape, Body Shape)
    positionalCorrection a b Manifold{..} = let
        percent = 0.4 -- usually 0.2 to 0.8
        slop = 0.05 -- usually 0.01 to 0.1 (to stop jitters)
        correction = ((max 0 (_mfPenetration - slop)) / (a^.inverseMass + b^.inverseMass)) * percent *^ _mfNormal
        a' = a & bPosition -~ (a^.inverseMass *^ correction)
        b' = b & bPosition +~ (b^.inverseMass *^ correction)
        in (a',b')

onDynamic :: Body Shape -> (Body Shape -> Body Shape) -> Body Shape
onDynamic a f = (if a^.bType == Dynamic then f else id) a

clearManifolds :: World -> IO World
clearManifolds w = return $ w & wManifolds .~ []

clearForces :: World -> IO World
clearForces w = do
    bodies <- A.mapArray (bForce .~ zero) (w^.wBodies)
    return $ w & wBodies .~ bodies

addBody :: Body Shape -> World -> IO (Int, World)
addBody a w@World{..} = do
    let key = head (w^.wUnusedBodyKeys)
    A.writeArray _wBodies key a
    let w' = w & (wUnusedBodyKeys %~ tail) . (wUsedBodyKeys %~ Set.insert key)
    return (key, w')

addBody' :: Body Shape -> World -> IO World
addBody' a w@World{..} = do
    let key = head (w^.wUnusedBodyKeys)
    A.writeArray _wBodies key a
    let w' = w & (wUnusedBodyKeys %~ tail) . (wUsedBodyKeys %~ Set.insert key)
    return w'

addBodyMay :: Body Shape -> World -> IO (Maybe (Int, World))
addBodyMay a w@World{..} = do
    let mkey = headMay (w^.wUnusedBodyKeys)
    case mkey of
        Nothing -> return Nothing
        Just key -> do
            A.writeArray _wBodies key a
            let w' = w & (wUnusedBodyKeys %~ tail) . (wUsedBodyKeys %~ Set.insert key)
            return $ Just (key, w')

deleteBody :: Int -> World -> World
deleteBody key = (wUnusedBodyKeys %~ (key:)) . (wUsedBodyKeys %~ Set.delete key)

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

newWorld :: Int -> IO World
newWorld maxBodies = do
    bodies <- A.newArray (0,maxBodies - 1) nullBody
    return $ World {
            _wBroadphase = (:[]),
            _wBodies = bodies,
            _wManifolds = [],
            _wUsedBodyKeys = Set.empty,
            _wUnusedBodyKeys = [0..maxBodies - 1],
            _wGravity = V2 0 9.8,
            _wDeltaTime = 1 / 60,
            _wIterations = 10
        }
 where
    nullBody = newCircle 0

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

rockEsque, woodEsque, metalEsque, bouncyBallEsque, superBallEsque, pillowEsque, staticEsque :: Body Shape -> Body Shape
rockEsque = bodyEsque 0.6 0.1
woodEsque = bodyEsque 0.3 0.2
metalEsque = bodyEsque 1.2 0.05
bouncyBallEsque = bodyEsque 0.3 0.8
superBallEsque = bodyEsque 0.3 0.95
pillowEsque = bodyEsque 0.1 0.2
staticEsque = bodyEsque 0 0.4

bodyEsque :: Float -> Float -> Body Shape -> Body Shape
bodyEsque d r a = a & (mass .~ massFromDensity d (a^.bShape)) . (bRestitution .~ r) 

massFromDensity :: Float -> Shape -> Float
massFromDensity density (ShapeRect r) = density * 4 * r^.rRadii^._x * r^.rRadii^._y
massFromDensity density (ShapeCircle c) = density * pi * (c^.cRadius) ^ 2 

uniformGrid :: V2 Int -> Float -> Broadphase
uniformGrid (V2 w h) bucketSide bodyPairs = zipWith testFilter tests (replicate (w*h) bodyPairs)
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
    v = V.fromListN len (replicate len [])
    in V.toList $ V.create $ inserts =<< V.thaw v
