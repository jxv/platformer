module Main where

import qualified Data.Map as M
import Control.Monad
import Control.Lens
import Linear
import Linear.Affine
import SDL
import Platformer
import Data.Word
import Foreign.C.Types
import Data.Default
import qualified Debug.Trace as D

data Demo = Demo {
    _demoRenderer :: Renderer,
    _demoWorld :: World,
    _demoControl :: Control
} deriving (Show)

data Control = Control {
    _ctrlQuit :: Bool,
    _ctrlLeft :: Bool,
    _ctrlUp :: Bool,
    _ctrlRight :: Bool,
    _ctrlDown :: Bool
} deriving (Show, Eq)

data Graphic = Graphic {
    _gRect :: Rectangle CInt,
    _gColor :: V4 Word8
}

makeClassy ''Demo
makeClassy ''Control
makeClassy ''Graphic

instance HasWorld Demo where
    world = demoWorld

instance HasControl Demo where
    control = demoControl

mkBodies :: [Body]
mkBodies = [
        newBody & (bRadii .~ V2 3 4) . (bPosition .~ V2 20 0) . friction,
{-
        newBody & (bRadii .~ V2 3 4) . (bPosition .~ V2 30 0),
        newBody & (bRadii .~ V2 3 4) . (bPosition .~ V2 35 0),
        newBody & (bRadii .~ V2 6 2) . (bPosition .~ V2 45 0),
-}
        newBody & (bRadii .~ V2 30 0.5) . (bPosition .~ V2 34 34) . (mass .~ 0) . friction
    ]
 where friction = (bDynamicFriction .~ 0.8) . (bStaticFriction .~ 0.8) . (bRestitution .~ 0.5)

mkWorld :: World
mkWorld = foldr addBody' (newWorld 1024) mkBodies

step :: Int -> World -> World
step n = foldr1 (.) (replicate n stepper)

main :: IO ()
main = do
    initialize (Just InitEverything)
    displays <- getDisplays
    let size :: V2 CInt
        size = fmap (flip div 2) (displayModeSize (head (displayModes (head displays))))
    window <- createWindow "demo" defaultWindow{ windowSize = size }
    ren <- createRenderer window (-1) defaultRenderer
    showWindow window
    screen <- getWindowSurface window
    mainLoop $ mkDemo ren
    freeSurface screen
    destroyRenderer ren
    destroyWindow window
    quit

mkDemo :: Renderer -> Demo
mkDemo ren = (Demo ren mkWorld def)

instance Default Control where
    def = Control False False False False False

pollEvents :: IO [Event]
pollEvents = do
    mevent <- pollEvent
    case mevent of
        Nothing -> return []
        Just event -> (event:) <$> pollEvents

readInput :: [EventPayload] -> Control -> Control
readInput [] ctrl = ctrl
readInput (e:es) ctrl = readInput es $ ctrl
    & (onPress ctrlQuit KeycodeEscape)
    . (onPress ctrlLeft KeycodeLeft)
    . (onPress ctrlUp KeycodeUp)
    . (onPress ctrlRight KeycodeRight)
    . (onPress ctrlDown KeycodeDown)
 where
    onPress l k = l %~ (|| keyPress k)
    keyPress key = case e of
        KeyboardEvent{..}-> keysymKeycode keyboardEventKeysym == key
        _ -> False

renderDemo :: Demo -> IO ()
renderDemo d = do
    setRenderDrawColor ren (V4 0x00 0x00 0x00 0xff) 
    renderClear ren
    mapM_ (renderGraphic ren . toGraphic maxBound) (d^.wBodies)
    renderPresent ren
 where
    ren = d^.demoRenderer

toGraphic :: V4 Word8 -> Body -> Graphic
toGraphic color a = let
    rect = fmap (round . (*10)) $ Rectangle (P (a^.bPosition - a^.bRadii)) (a^.bRadii * 2)
    in Graphic rect color 

renderGraphic :: Renderer -> Graphic -> IO ()
renderGraphic ren g = do
    setRenderDrawColor ren (g^.gColor)
    renderDrawRect ren (g^.gRect)

mainLoop :: Demo -> IO ()
mainLoop d = do
    startTick <- ticks
    events <- pollEvents
    let c = readInput (fmap eventPayload events) (d^.control)
    let d' = d & world %~ stepper
    renderDemo d'
--    print $ d^.wManifolds
    endTick <- ticks
    delay (delayTime 16 startTick endTick)
    unless (c^.ctrlQuit) (mainLoop d')
{-
    let Input{..} = readInput (fmap eventPayload events) def
    let motion = 150
    let orZero b v n = n + if b then v else 0
    let move v = v
            & (_x %~ (orZero inputLeft $ -motion))
            . (_y %~ (orZero inputUp $ -motion))
            . (_x %~ (orZero inputRight motion))
            . (_y %~ (orZero inputDown motion))
            . (_y %~ (orZero inputDown motion))
    let d' = d & (world %~ worldStep) . (wBodies %~ (M.adjust (bForce %~ move) 1))
-}

delayTime :: Word32 -> Word32 -> Word32 -> Word32
delayTime goal start end = let
    frame = end - start
    in if frame >= goal then 0 else goal - frame
