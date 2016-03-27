-- https://github.com/Rydgel/flappy-haskell/
{-# LANGUAGE OverloadedStrings #-}

module Rendering (animate) where

import           Control.Monad
import           Control.Concurrent
import           Data.Text           (Text)
import           Data.Word
import           FRP.Yampa
import           Linear              hiding (identity)
import           SDL                 (($=))
import qualified SDL

import Game

backgroundColor :: V4 Word8
backgroundColor = V4 55 201 215 maxBound

animate :: Text
        -> Int
        -> Int
        -> SF (Event SDL.EventPayload) (Game, Bool)
        -> IO ()
animate title ww wh sf = do
    SDL.initialize [SDL.InitVideo, SDL.InitAudio]
    SDL.HintRenderScaleQuality $= SDL.ScaleBest
    window <- SDL.createWindow title windowConf
    SDL.showWindow window
    renderer <- SDL.createRenderer window (-1) renderConf
    SDL.rendererDrawColor renderer $= backgroundColor
    --textures <- loadTextures renderer
    --digitsTextures <- loadDigitsTextures renderer

    lastInteraction <- newMVar =<< SDL.time

    let senseInput _canBlock = do
          currentTime <- SDL.time
          dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
          mEvent <- SDL.pollEvent
          return (dt, Event . SDL.eventPayload <$> mEvent)

        renderOutput changed (gameState, shouldExit) = do
          when changed $ do
              SDL.clear renderer
              --renderDisplay renderer textures digitsTextures (fromIntegral wh) gameState
              --renderSounds audios gameState
              SDL.present renderer
          return shouldExit

    reactimate (return NoEvent) senseInput renderOutput sf

    --destroyTextures textures
    --destroyAudios audios
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

    where
      windowConf = SDL.defaultWindow
         { SDL.windowInitialSize =
             V2 (fromIntegral ww) (fromIntegral wh)
         , SDL.windowOpenGL = Just SDL.defaultOpenGL
         }
      renderConf = SDL.RendererConfig
         { SDL.rendererType = SDL.AcceleratedVSyncRenderer
         , SDL.rendererTargetTexture = False
         }
