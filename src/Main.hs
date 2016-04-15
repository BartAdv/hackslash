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
  test
  -- g <- newStdGen
  -- animate "Yamplo" 640 480 $
  --   parseWinInput >>> (game g &&& handleExit)
