{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game where

import           FRP.Yampa
import           Prelude             hiding (init)

import Input

data Game = Game { foobar :: Int }

game :: RandomGen g => g -> SF AppInput Game
game rng = proc input -> do
  returnA -< Game 1

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent
