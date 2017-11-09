{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Freeablo
import Game
import Types
import Reflex.SDL2

main :: IO ()
main = do
  initFAIO
  renderer <- initRenderer 1920 1080 False
  spriteManager <- createSpriteManager
  town <- createTownLevel
  initialize [InitTimer]
  host () (game spriteManager town)
  -- TODO: finalize/free
