{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Freeablo
import Game
import Types
import Reflex.SDL.Host
import SDL

main :: IO ()
main = do
  -- initFAIO
  -- renderer <- initRenderer 1920 1080 False
  -- spriteManager <- createSpriteManager
  -- town <- createTownLevel
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  sdlHost (Just 25) (game undefined undefined)
  -- TODO: finalize/free
