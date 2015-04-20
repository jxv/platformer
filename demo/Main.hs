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
    _gPoints :: VS.Vector (Point V2 CInt),
    _gColor :: V4 Word8
}

makeClassy ''Demo
makeClassy ''Control
makeClassy ''Graphic

class ToGraphic a where
    toGraphic :: a -> Graphic

instance HasWorld Demo where
    world = demoWorld

instance HasControl Demo where
    control = demoControl


mkBodies :: [Body Shape]
mkBodies = [
        newCircle 3 & (bPosition .~ V2 25 (-20)),
        newCircle 3 & (bPosition .~ V2 25 (-10)),
        newCircle 3 & (bPosition .~ V2 10 17.5) . (mass .~ 10) . (bVelocity .~ V2 9.8 0),
        newRect (V2 3 4) & (bPosition .~ V2 30 (-10)),
        newRect (V2 3 4) & (bPosition .~ V2 30 0),
        newRect (V2 3 4) & (bPosition .~ V2 30 0),
        newRect (V2 6 2) & (bPosition .~ V2 45 0),
        newRect (V2 30 0.5) & (bPosition .~ V2 34 34) . (mass .~ 0),
        newRect (V2 10 0.5) & (bPosition .~ V2 13 20) . (mass .~ 0)
    ]

cannonBall = newCircle 2 & (bPosition .~ V2 (-20) 30) . (bVelocity .~ V2 200 0) . (mass .~ 80) . (bRestitution .~ 0.1)

mkWorld :: World
mkWorld = (foldr addBody' (newWorld 4096) (cannonBall : (mkLogo $ V2 10 6))) & wGravity .~ V2 0 0
   

step :: Int -> World -> World
step n = foldr1 (.) (replicate n stepper)

main :: IO ()
main = do
    initialize (Just InitEverything)
    window <- createWindow "demo" defaultWindow
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
    mapM_ (renderGraphic ren . toGraphic) (d^.wBodies)
    renderPresent ren
 where
    ren = d^.demoRenderer

instance ToGraphic (Body Shape) where
    toGraphic a@Body{_bShape = ShapeRect r} = toGraphic (r <$ a)
    toGraphic a@Body{_bShape = ShapeCircle c} = toGraphic (c <$ a)

instance ToGraphic (Body Rect) where
    toGraphic a = let
        Aabb{..} = toAabb a
        pts  = VS.fromListN 5 $ map (P . fmap round . (*10)) [
                _aMin,
                V2 (_aMin^._x) (_aMax^._y),
                _aMax,
                V2 (_aMax^._x) (_aMin^._y),
                _aMin
            ]
        in Graphic pts maxBound

instance ToGraphic (Body Circle) where
    toGraphic a = let
        tau = 2 * pi
        cen = fmap round (a^.bPosition)
        segs = 25
        angles = fmap (\i -> fromIntegral i * tau / fromIntegral (segs - 1)) [0..(segs - 1)]
        cons theta = P $ fmap (round . (*10)) ((a^.bPosition) + V2 (cos theta * (a^.cRadius)) (sin theta * (a^.cRadius)))
        pts = VS.fromListN segs $ fmap cons angles
        in Graphic pts maxBound

renderGraphic :: Renderer -> Graphic -> IO ()
renderGraphic ren Graphic{..} = do
    setRenderDrawColor ren _gColor
    renderDrawLines ren _gPoints

mainLoop :: Demo -> IO ()
mainLoop d = do
    startTick <- ticks
    events <- pollEvents
    let c = readInput (fmap eventPayload events) (d^.control)
    let d' = d & world %~ stepper
    renderDemo d'
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

{-
Rock       Density : 0.6  Restitution : 0.1
Wood       Density : 0.3  Restitution : 0.2
Metal      Density : 1.2  Restitution : 0.05
BouncyBall Density : 0.3  Restitution : 0.8
SuperBall  Density : 0.3  Restitution : 0.95
Pillow     Density : 0.1  Restitution : 0.2
Static     Density : 0.0  Restitution : 0.4
-}
        

mkLogo :: V2 Float -> [Body Shape]
mkLogo offset =
    [
        shape & bPosition .~ (offset + V2 (fromIntegral x) (fromIntegral y))
    | 
        (y,line) <- zip [0..] logo,
        (x,ch) <- zip [0..] line,
        ch == '.',
        let shape = if mod x 2 == 0 then newRect (V2 0.48 0.48) else newCircle 0.48
    ]

logo :: [[Char]]
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
