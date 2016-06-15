{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           FRP.Yampa
import           Game
import           Input
import           Rendering
import           System.Random (newStdGen)

import Test

main :: IO ()
main = do
  g <- newStdGen
  (window, renderer) <- initializeSDL
  animate renderer $ parseWinInput >>> (game g &&& handleExit)
  finalizeSDL (window, renderer)
