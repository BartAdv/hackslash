-- https://github.com/Rydgel/flappy-haskell/
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Rendering
       (initializeSDL
       ,finalizeSDL
       ,animate) where

import           Control.Monad
import           Control.Concurrent
import           Data.Word
import           FRP.Yampa
import Linear.Affine hiding (origin)
import Linear.V2
import Linear.V4
import           SDL                 (Renderer, ($=))
import qualified SDL

import Game
import Graphics
import Level

renderLevel :: Renderer -> Level -> Point V2 Int -> IO ()
renderLevel renderer Level{..} center = undefined
  --mapM renderPillar levelPillars
  where
    renderPillar (x,y) (Just PillarTexture{..}) =
      SDL.copy renderer pillarTexture Nothing (Just $ SDL.Rectangle (P (V2 x y)) pillarTextureSize)
    renderPillar _ Nothing = return ()

isoToScreen :: Num a => Point V2 a -> Point V2 a -> Point V2 a
isoToScreen origin (P (V2 x y)) = origin + P (V2 ((x - y) * 32) ((x + y) * 16))

screenToIso :: Fractional a => Point V2 a -> Point V2 a -> Point V2 a
screenToIso origin coords = P (V2 ix iy)
  where P (V2 x y) = coords - origin
        ix = (x / 32 + y / 16) / 2
        iy = (y / 16 - x / 32) / 2

animate :: Renderer -> SF (Event SDL.EventPayload) (Game, Bool) -> IO ()
animate renderer sf = do
  lastInteraction <- newMVar =<< SDL.time
  reactimate (return NoEvent) (senseInput lastInteraction) (renderOutput renderer) sf

  where
    senseInput lastInteraction _canBlock = do
      currentTime <- SDL.time
      dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
      mEvent <- SDL.pollEvent
      return (dt, Event . SDL.eventPayload <$> mEvent)

    renderOutput renderer changed (gameState, shouldExit) = do
      when changed $ do
       SDL.clear renderer
       --renderDisplay renderer textures digitsTextures (fromIntegral wh) gameState
       --renderSounds audios gameState
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
      { SDL.windowInitialSize = V2 640 480
      , SDL.windowOpenGL = Just SDL.defaultOpenGL
      }

    renderConf = SDL.RendererConfig
      { SDL.rendererType = SDL.AcceleratedVSyncRenderer
      , SDL.rendererTargetTexture = False
      }

    backgroundColor :: V4 Word8
    backgroundColor = V4 12 42 100 maxBound

finalizeSDL :: (SDL.Window, SDL.Renderer) -> IO ()
finalizeSDL (window, renderer) = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
