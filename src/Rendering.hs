-- https://github.com/Rydgel/flappy-haskell/
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Rendering
       (initializeSDL
       ,finalizeSDL
       ,renderGame) where

import Control.Monad
import Control.Concurrent
import Data.Word
import Data.Vector ((!))
import Linear.Affine hiding (origin)
import Linear.V2
import Linear.V4
import SDL                 (Renderer, ($=))

import qualified SDL

import Assets
import Game
import Graphics
import Level

drawingCoords :: Point V2 Int -> V2 Int -> [Point V2 Int]
drawingCoords (P (V2 fromX fromY)) (V2 colCount rowCount) =
  [P (V2 (fromX + x + y `div` 2) (fromY + y `div` 2 + y `mod` 2 - x)) | y <- [0..rowCount*2 - 1], x <- [0..colCount-1]]

isoToScreen :: Num a => Point V2 a -> Point V2 a -> Point V2 a -> Point V2 a
isoToScreen offset cameraPos coords =
  let (P (V2 x y)) = coords - cameraPos
  in P (V2 ((x - y) * 32) ((x + y) * 16)) + offset

screenToIso offset cameraPos coords = P (V2 ix iy) + cameraPos
  where
    (P (V2 x y)) = coords - offset
    ix = (x `div` 32 + y `div` 16) `div` 2
    iy = (y `div` 16 - x `div` 32) `div` 2

cameraPos = P (V2 12 29)
drawArea = V2 20 30
screenCenter = P (V2 512 384)

renderLevel :: Renderer -> Level -> Point V2 Int -> IO ()
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

renderGame :: Renderer -> Assets -> Game -> IO ()
renderGame renderer Assets{..} _gameState = do
  SDL.rendererDrawColor renderer $= backgroundColor
  SDL.clear renderer
  SDL.rendererDrawColor renderer $= V4 255 255 255 maxBound
  renderLevel renderer assetsLevel cameraPos
  SDL.present renderer

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
