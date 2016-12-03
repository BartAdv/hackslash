{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Freeablo
import Game
import Types
import Reflex.SDL.Host

main :: IO ()
main = do
  initFAIO
  renderer <- initRenderer 1920 1080 False
  spriteManager <- createSpriteManager
  town <- createTownLevel
  sdlHost (Just ticksPerSecond) (game spriteManager town)
  -- TODO: finalize/free
