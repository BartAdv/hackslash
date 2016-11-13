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
import Reflex
import Rendering
import SDLEvent
import SDLEventLoop
import SDL hiding (Event)

import Graphics
import Assets
import Types

ticksPerSecond :: Int
ticksPerSecond = 60

data Game t = Game {
  gameCameraPos :: Behavior t Coord,
  -- boring game
  gameMonster :: Monster t
}

type Health = Int

data Monster t = Monster {
  monsterAnimation :: Cl2Anim,
  monsterHealth :: Behavior t Health,
  monsterAnimationFrame :: Behavior t Int,
  monsterCoords :: Behavior t Coord
}

data Input t = Input {
  inputTick :: Event t Word32
}

testMonster :: (Reflex t, MonadFix m, MonadHold t m)
            => Assets
            -> Input t
            -> m (Monster t)
testMonster Assets{..} Input{..} = do
  frame <- accum (+) 0 (1 <$ inputTick)
  let coords = constant (P (V2 20 20))
  pure $ Monster assetsTest undefined frame coords

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

game :: Renderer -> Assets -> SDLApp t m
game renderer assets sel = do
  let keyPress = fmap (keysymKeycode . keyboardEventKeysym) .
                 ffilter ((== Pressed) . keyboardEventKeyMotion) .
                 select sel $
                 SDLKeyboard
      eQuit = void $ ffilter (== KeycodeEscape) keyPress
      tick = select sel SDLTick
  cameraPos <- screenScroll (P (V2 15 29)) keyPress
  monster <- testMonster assets (Input tick)
  let game' = Game cameraPos monster
  performEvent_ $ renderGame renderer assets game' <$ tick
  performEvent_ $ liftIO quit <$ eQuit
  return eQuit

-- need to figure out correct monad stack, this looks tedious with all the liftIO
renderGame :: (Reflex t, MonadSample t m, MonadIO m)
           => Renderer
           -> Assets
           -> Game t
           -> m ()
renderGame renderer Assets{..} Game{..} = do
  liftIO $ clearFrame renderer
  cameraPos <- sample gameCameraPos
  frame <- sample (monsterAnimationFrame gameMonster)
  liftIO $ do
    renderLevel renderer assetsLevel cameraPos
    renderAnimation renderer (monsterAnimation gameMonster) (P (V2 50 50)) frame
  liftIO $ showFrame renderer