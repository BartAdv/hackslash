{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Assets
import Game
import Rendering
import SDLEventLoop

main :: IO ()
main = do
  (window, renderer) <- initializeSDL
  assets <- loadAssets renderer "diabdat/levels/towndata" "foo"
  sdlHost renderer (Just 60) (game assets)
  finalizeSDL (window, renderer)
