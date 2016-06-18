-- https://github.com/Rydgel/flappy-haskell/
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Rendering
       (initializeSDL
       ,finalizeSDL
       ,animate) where

import Control.Monad
import Control.Concurrent
import Data.Word
import Data.Vector ((!))
import FRP.Yampa
import Linear.Affine hiding (origin)
import Linear.V2
import Linear.V4
import SDL                 (Renderer, ($=))

import qualified SDL

import Assets
import Game
import Graphics
import Level

drawingCoords :: [Point V2 Int]
drawingCoords = [P (V2 x y) | y <- [1..30], x <- [1..30]]

isoToScreen :: Num a => Point V2 a -> Point V2 a -> Point V2 a
isoToScreen origin (P (V2 x y)) = origin + P (V2 ((x - y) * 32) ((x + y) * 16))

screenToIso :: Fractional a => Point V2 a -> Point V2 a -> Point V2 a
screenToIso origin coords = P (V2 ix iy)
  where P (V2 x y) = coords - origin
        ix = (x / 32 + y / 16) / 2
        iy = (y / 16 - x / 32) / 2

renderLevel :: Renderer -> Level -> Point V2 Int -> IO ()
renderLevel renderer Level{..} center =
  forM_ drawingCoords $ \coord@(P (V2 x y)) ->
    let pillar = levelPillars ! (y * levelWidth + x)
        screenCoords = fromIntegral <$> isoToScreen center coord
    in renderPillar screenCoords pillar
  where
    renderPillar coords (Just PillarTexture{..}) = do
      let V2 _ h = pillarTextureSize
          offset = h - 32
          coords' = coords - P (V2 0 offset)
      SDL.copy renderer pillarTexture Nothing (Just $ SDL.Rectangle coords' pillarTextureSize)
    renderPillar _ Nothing = return ()

backgroundColor :: V4 Word8
backgroundColor = V4 12 42 100 maxBound

animate :: Assets -> Renderer -> SF (Event SDL.EventPayload) (Game, Bool) -> IO ()
animate Assets{..} renderer sf = do
  lastInteraction <- newMVar =<< SDL.time
  reactimate (return NoEvent) (senseInput lastInteraction) renderOutput  sf

  where
    senseInput lastInteraction _canBlock = do
      currentTime <- SDL.time
      dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
      mEvent <- SDL.pollEvent
      return (dt, Event . SDL.eventPayload <$> mEvent)

    renderOutput changed (gameState, shouldExit) = do
      when changed $ do
        SDL.rendererDrawColor renderer $= backgroundColor
        SDL.clear renderer
        SDL.rendererDrawColor renderer $= V4 255 255 255 maxBound
        renderLevel renderer assetsLevel (P $ V2 0 0)
        SDL.present renderer
      return shouldExit

initializeSDL :: IO (SDL.Window, SDL.Renderer)
initializeSDL = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleBest
  window <- SDL.createWindow "yamplo" windowConf
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
