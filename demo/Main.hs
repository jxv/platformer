{-# LANGUAGE FlexibleInstances #-}
module Main where

import qualified Data.Map as M
import qualified Data.Vector.Storable as VS
import Control.Applicative
import Control.Monad
import Control.Lens
import Linear
import Linear.Affine
import SDL
import Platformer
import Data.Word
import Foreign.C.Types
import Data.Default
import Data.Maybe
import qualified Debug.Trace as D

data Demo = Demo {
    _demoRenderer :: Renderer,
    _demoWorld :: World,
    _demoControl :: Control,
    _demoCircle :: Texture,
    _demoBox :: Texture,
    _demoCannonBall :: Texture
}

data Control = Control {
    _ctrlQuit :: Bool,
    _ctrlLeft :: Bool,
    _ctrlUp :: Bool,
    _ctrlRight :: Bool,
    _ctrlDown :: Bool
} deriving (Show, Eq)

makeClassy ''Demo
makeClassy ''Control

instance HasWorld Demo where
    world = demoWorld

instance HasControl Demo where
    control = demoControl

instance Default Control where
    def = Control False False False False False

main :: IO ()
main = do
    initialize (Just InitEverything)
    hinSet <- setHint HintRenderScaleQuality ScaleLinear
    window <- createWindow "demo" defaultWindow
    ren <- createRenderer window (-1) defaultRenderer{ rendererPresentVSync = True }
    showWindow window
    screen <- getWindowSurface window
    cirTex <- createCircleTex ren 1
    canTex <- createCircleTex ren cannonBallRadius
    boxTex <- createBoxTex ren
    let demo = mkDemo ren cirTex boxTex canTex
    mainLoop demo
    destroyTexture boxTex
    destroyTexture canTex
    destroyTexture cirTex
    freeSurface screen
    destroyRenderer ren
    destroyWindow window
    quit

createCircleTex :: Renderer -> Float -> IO Texture
createCircleTex ren rad = do
    tex <- createTexture ren RGBA8888 TextureAccessTarget (fmap floor (10 ^* rad))
    setRenderTarget ren (Just tex)
    setRenderDrawColor ren minBound
    renderClear ren
    setRenderDrawColor ren maxBound
    renderDrawLines ren $ let
        radius = rad * 5 - 0.5
        center = pure radius
        tau = 2 * pi
        segs = 16
        angles = fmap (\i -> fromIntegral i * tau / fromIntegral (segs - 1)) [0..(segs - 1)]
        cons theta = P $ fmap round (center + V2 (cos theta * radius) (sin theta * radius))
        in VS.fromListN segs $ fmap cons angles
    setRenderTarget ren Nothing
    return tex

createBoxTex :: Renderer -> IO Texture
createBoxTex ren = do
    tex <- createTexture ren RGBA8888 TextureAccessTarget (V2 10 10)
    setRenderTarget ren (Just tex)
    setRenderDrawColor ren minBound
    renderClear ren
    setRenderDrawColor ren maxBound
    renderDrawRect ren $ Rectangle (P $ V2 0 0) (V2 10 10)
    setRenderTarget ren Nothing
    return tex

mkDemo :: Renderer -> Texture -> Texture -> Texture -> Demo
mkDemo ren cir box cann = Demo ren mkWorld def cir box cann

mkWorld :: World
mkWorld = (foldr addBody' (newWorld 2048) mkBodies) & wGravity .~ V2 0 0

mkBodies :: [Body Shape]
mkBodies = cannonBall : (mkLogo (V2 10 6))

cannonBallRadius = 5

cannonBall :: Body Shape
cannonBall = newCircle 5 & (bPosition .~ V2 (-50) 30) . (bVelocity .~ V2 200 0) . (mass .~ 80) . (bRestitution .~ 0.05)

mkLogo :: V2 Float -> [Body Shape]
mkLogo offset = 
    [ shape & bPosition .~ (offset + V2 (fromIntegral x) (fromIntegral y))
    | (y,line) <- zip [0..] logo
    , (x,ch) <- zip [0..] line
    , ch == '.'
    , let shape = if mod x 2 == 0 then newRect (V2 0.48 0.48) else newCircle 0.48
    ]
 where
    logo = [
        "............   ............",
        " ............   ............",
        " ............   ............",
        "  ............   ............",
        "   ............   ............",
        "   ............   ............",
        "    ............   ............",
        "     ............   ............",
        "     ............   ............",
        "      ............   ............",
        "       ............   ............",
        "       ............   ............",
        "        ............   ............",
        "         ............   ............",
        "         ............   ............",
        "          ............   ............   ........................",
        "          ............   ............   ........................",
        "           ............   ............   .......................",
        "            ............   ............   ......................",
        "            ............   ............   ......................",
        "             ............   ............   .....................",
        "              ............   ............   ....................",
        "              ............   ............   ....................",
        "               ............   ............",
        "               ............   ............",
        "              ............   ..............   ..................",
        "              ............   ..............   ..................",
        "             ............   ................   .................",
        "            ............   ..................   ................",
        "            ............   ..................   ................",
        "           ............   ....................   ...............",
        "          ............   ......................   ..............",
        "          ............   ......................    .............",
        "         ............   ............ ...........",
        "        ............   ............   ...........",
        "        ............   ............   ...........",
        "       ............   ............     ...........",
        "      ............   ............       ...........",
        "      ............   ............       ...........",
        "     ............   ............         ...........",
        "    ............   ............           ...........",
        "    ............   ............           ...........",
        "   ............   ............             ...........",
        "  ............   ............               ...........",
        "  ............   ............               ...........",
        " ............   ............                 ...........",
        "............   ............                   ...........",
        "............   ............                   ..........."
        ]

mainLoop :: Demo -> IO ()
mainLoop d = do
    startTick <- ticks
    events <- pollEvents
    let c = readInput (fmap eventPayload events) (d^.control)
    let d' = d & world %~ stepper
    renderDemo d'
    endTick <- ticks
    print $ endTick - startTick
    delay (delayTime 16 startTick endTick)
    unless (c^.ctrlQuit) (mainLoop d')

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
    forM_ (M.toList $ d^.wBodies) $ \(akey,a) -> do
        let (tex, dim) = case a^.bShape of
                ShapeRect _ -> (d^.demoBox, 10)
                ShapeCircle a ->
                    if a^.cRadius == cannonBallRadius
                    then (d^.demoCannonBall, fmap round $ 10 ^* cannonBallRadius)
                    else (d^.demoCircle, 10)
        let pos = fmap (round . (*10)) (a^.bPosition)
        renderCopy ren tex Nothing (Just $ Rectangle (P pos) dim)
    renderPresent ren
 where
    ren = d^.demoRenderer

delayTime :: Word32 -> Word32 -> Word32 -> Word32
delayTime goal start end = let
    frame = end - start
    in if frame >= goal then 0 else goal - frame

{-
Rock       Density : 0.6  Restitution : 0.1
Wood       Density : 0.3  Restitution : 0.2
Metal      Density : 1.2  Restitution : 0.05
BouncyBall Density : 0.3  Restitution : 0.8
SuperBall  Density : 0.3  Restitution : 0.95
Pillow     Density : 0.1  Restitution : 0.2
Static     Density : 0.0  Restitution : 0.4
-}
