{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import Reflex ()
import ReflexSDL

data Game = Game { foobar :: Int }

game :: SDLApp t m Game
game _sdlEv = pure $ pure $ Game 21