{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Game (game) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Reflex
import Rendering
import SDLEvent
import SDLEventLoop
import SDL hiding (SDLEvent)

import Assets
import Types

game :: Assets -> SDLApp t m
game assets sel = do
  let game = Game { foobar = 21 }
      keyPress = fmap (keysymKeycode . keyboardEventKeysym) .
                 ffilter ((== Pressed) . keyboardEventKeyMotion) .
                 select sel $
                 SDLKeyboard
      eQuit = void . ffilter (== KeycodeEscape) $ keyPress
      tick = select sel SDLTick
      gameState = constant (Game { foobar = 21 })
  renderer <- ask
  performEvent_ $ liftIO . renderGame renderer assets <$> tag gameState tick
  performEvent_ $ liftIO quit       <$  eQuit
  return eQuit
