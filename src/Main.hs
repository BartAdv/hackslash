{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           FRP.Yampa
import           System.Random (newStdGen)

import Game
import Input
import Rendering
import Assets

main :: IO ()
main = do
  g <- newStdGen
  (window, renderer) <- initializeSDL
  assets <- loadAssets renderer "diabdat/levels/towndata" "foo"
  animate assets renderer $ parseWinInput >>> (game g &&& handleExit)
  finalizeSDL (window, renderer)
