-- https://github.com/Rydgel/flappy-haskell/
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Rendering
       (initializeSDL
       ,finalizeSDL
       ,renderLevel
       ,renderAnimation
       ,clearFrame
       ,showFrame) where

import Control.Monad
import Data.Word
import Data.Vector ((!))
import Linear.Affine hiding (origin)
import Linear.V2
import Linear.V4
import SDL                 (Renderer, ($=))

import qualified SDL

import Graphics
import Level
import Types

drawingCoords :: Coord -> V2 Int -> [Point V2 Int]
drawingCoords (P (V2 fromX fromY)) (V2 colCount rowCount) =
  [P (V2 (fromX + x + y `div` 2) (fromY + y `div` 2 + y `mod` 2 - x)) | y <- [0..rowCount*2 - 1], x <- [0..colCount-1]]

isoToScreen :: Num a => Point V2 a -> Point V2 a -> Point V2 a -> Point V2 a
isoToScreen offset cameraPos coords =
  let (P (V2 x y)) = coords - cameraPos
  in P (V2 ((x - y) * 32) ((x + y) * 16)) + offset

screenToIso :: Integral a => Point V2 a -> Point V2 a -> Point V2 a -> Point V2 a
screenToIso offset cameraPos coords = P (V2 ix iy) + cameraPos
  where
    (P (V2 x y)) = coords - offset
    ix = (x `div` 32 + y `div` 16) `div` 2
    iy = (y `div` 16 - x `div` 32) `div` 2

drawArea :: V2 Int
drawArea = V2 20 30
screenCenter :: Coord
screenCenter = P (V2 512 384)

renderAnimation :: Renderer -> Cl2Anim -> Coord -> Int -> IO ()
renderAnimation renderer anim coords frameNum =
  SDL.copy renderer animationTexture src dest
  where
    src = Just $ SDL.Rectangle (P (V2 (fromIntegral frameNum' * frameWidth) 0)) frameSize
    dest = Just $ SDL.Rectangle (fromIntegral <$> coords) frameSize
    frameNum' = frameNum `mod` frameCount
    Cl2Anim animationTexture frameCount frameSize@(V2 frameWidth _) = anim

renderLevel :: Renderer -> Level -> Coord -> IO ()
renderLevel renderer Level{..} cameraPos =
  forM_ (drawingCoords corner drawArea) $ \coord@(P (V2 x y)) ->
    let pillar = levelPillars ! (y * levelWidth + x)
        screenCoords = fromIntegral <$> isoToScreen screenCenter cameraPos coord
    in renderPillar screenCoords pillar
  where
    corner = screenToIso screenCenter cameraPos (P (V2 0 0))
    renderPillar coords (Just PillarTexture{..}) = do
      let V2 _ h = pillarTextureSize
          offset = h - 32
          coords' = coords - P (V2 0 offset)
      SDL.copy renderer pillarTexture Nothing (Just $ SDL.Rectangle coords' pillarTextureSize)
    renderPillar _ Nothing = return ()

backgroundColor :: V4 Word8
backgroundColor = V4 12 42 100 maxBound

clearFrame :: Renderer -> IO ()
clearFrame renderer = do
  SDL.rendererDrawColor renderer $= backgroundColor
  SDL.clear renderer
  SDL.rendererDrawColor renderer $= V4 255 255 255 maxBound

showFrame :: Renderer -> IO ()
showFrame = SDL.present

initializeSDL :: IO (SDL.Window, SDL.Renderer)
initializeSDL = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleBest
  window <- SDL.createWindow "hackslash" windowConf
  SDL.showWindow window
  renderer <- SDL.createRenderer window (-1) renderConf
  SDL.rendererDrawColor renderer $= backgroundColor
  SDL.clear renderer
  SDL.present renderer
  return (window, renderer)

  where
    windowConf = SDL.defaultWindow
      { SDL.windowInitialSize = V2 1024 768
      , SDL.windowOpenGL = Just SDL.defaultOpenGL
      }

    renderConf = SDL.RendererConfig
      { SDL.rendererType = SDL.AcceleratedVSyncRenderer
      , SDL.rendererTargetTexture = False
      }

finalizeSDL :: (SDL.Window, SDL.Renderer) -> IO ()
finalizeSDL (window, renderer) = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
