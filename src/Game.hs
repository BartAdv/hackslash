{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Game where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import GHC.Word (Word32)
import Linear.V2
import Linear.Affine
import Reflex
import SDLEvent
import SDLEventLoop
import SDL hiding (Renderer, Event)

import Freeablo
import Types

import Debug.Trace

ticksPerSecond :: Int
ticksPerSecond = 60

data Game t = Game {
  gameCameraPos :: Behavior t Coord,
  -- boring game
  gameMonster :: Monster t
}

type Health = Int

data Monster t = Monster {
  monsterHealth :: Behavior t Health,
  monsterAnimationFrame :: Behavior t Int,
  monsterCoords :: Behavior t Coord
}

data Input t = Input {
  inputTick :: Event t Word32
}

testMonster :: (Reflex t, MonadFix m, MonadHold t m)
            => Input t
            -> m (Monster t)
testMonster Input{..} = do
  frame <- accum (+) 0 (1 <$ inputTick)
  let coords = constant (P (V2 20 20))
  pure $ Monster undefined frame coords

data GameState = GameState {
  gameStateCameraPos :: Coord
}

newGame = GameState { gameStateCameraPos = P (V2 15 29) }

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

game :: Renderer -> Level -> SDLApp t m
game renderer level sel = do
  let keyPress = fmap (keysymKeycode . keyboardEventKeysym) .
                 ffilter ((== Pressed) . keyboardEventKeyMotion) .
                 select sel $
                 SDLKeyboard
      eQuit = void $ ffilter (== KeycodeEscape) keyPress
      tick = select sel SDLTick
  cameraPos <- screenScroll (P (V2 50 50)) keyPress
  monster <- testMonster (Input tick)
  let game' = Game cameraPos monster
  performEvent_ $ renderGame renderer level game' <$ tick
  performEvent_ $ liftIO quit <$ eQuit
  return eQuit

-- need to figure out correct monad stack, this looks tedious with all the liftIO
renderGame :: (Reflex t, MonadSample t m, MonadIO m)
           => Renderer
           -> Level
           -> Game t
           -> m ()
renderGame renderer level Game{..} = do
  cameraPos@(P (V2 x y)) <- sample gameCameraPos
  renderFrame renderer level cameraPos
