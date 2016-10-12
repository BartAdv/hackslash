{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Game (game) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Fix (MonadFix)
import Reflex
import Rendering
import SDLEvent
import SDLEventLoop
import SDL hiding (Event)

import Assets
import Types

data Game t = Game {
  cameraPos :: Behavior t Coord
}

type Health = Int

data Monster t = Monster {
  health :: Behavior t Health,
  animationFrame :: Behavior t Int
}

data GameState = GameState {
  stateCameraPos :: Coord
}

newGame = GameState { stateCameraPos = P (V2 15 29) }

screenScroll :: (Reflex t, MonadHold t m, MonadFix m)
             => Coord
             -> Event t Keycode
             -> m (Behavior t Coord)
screenScroll initialPos keyPress = accum (\pos d -> pos + P d) initialPos cameraMove
  where
    moveLeft   = V2 (-1) 1    <$ ffilter (== KeycodeLeft) keyPress
    moveRight  = V2 1 (-1)    <$ ffilter (== KeycodeRight) keyPress
    moveUp     = V2 (-1) (-1) <$ ffilter (== KeycodeUp) keyPress
    moveDown   = V2 1 1       <$ ffilter (== KeycodeDown) keyPress
    cameraMove = leftmost [moveLeft, moveRight, moveUp, moveDown]

game :: Renderer -> Assets -> SDLApp t m
game renderer assets sel = do
  let keyPress = fmap (keysymKeycode . keyboardEventKeysym) .
                 ffilter ((== Pressed) . keyboardEventKeyMotion) .
                 select sel $
                 SDLKeyboard
      eQuit = void $ ffilter (== KeycodeEscape) keyPress
      tick = select sel SDLTick
  cameraPos <- screenScroll (P (V2 15 29)) keyPress
  performEvent_ $ liftIO . renderGame renderer assets <$> tag cameraPos tick
  performEvent_ $ liftIO quit <$ eQuit
  return eQuit
