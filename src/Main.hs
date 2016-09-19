{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Assets
import Game
import Rendering
import ReflexSDL

main :: IO ()
main = do
  (window, renderer) <- initializeSDL
  assets <- loadAssets renderer "diabdat/levels/towndata" "foo"
  host game (renderGame renderer assets)
  finalizeSDL (window, renderer)
