{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Assets
import Game
import Rendering
import SDLEventLoop

main :: IO ()
main = do
  (window, renderer) <- initializeSDL
  assets <- loadAssets "diabdat" renderer "foo"
  sdlHost (Just 60) (game renderer assets)
  finalizeSDL (window, renderer)
