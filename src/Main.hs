{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Freeablo
import Game
import Types
import SDLEventLoop

main :: IO ()
main = do
  initFAIO
  renderer <- createRenderer 1200 1080 False
  town <- createTownLevel
  sdlHost (Just ticksPerSecond) (game renderer town)
  -- finalize
