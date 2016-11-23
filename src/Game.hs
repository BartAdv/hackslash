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
import Reflex.SDL.Event
import Reflex.SDL.Host
import SDL hiding (Renderer, Event)

import Freeablo
import Types

import Debug.Trace

ticksPerSecond :: Int
ticksPerSecond = 60

data Game t = Game {
  gameCameraPos :: Behavior t Coord,
  gameMonsters :: [Monster t]
}

type Health = Int

data Monster t = Monster
  { monsterAnim :: SpriteGroup
  , monsterHealth :: Behavior t Health
  , monsterAnimationFrame :: Dynamic t Int
  , monsterPos :: Dynamic t Coord
  , monsterDie :: Event t () }

data Input t = Input
  { inputTick :: Event t Word32}

testMonster :: (Reflex t, MonadFix m, MonadHold t m, MonadIO m)
            => SpriteManager
            -> Input t
            -> m (Monster t)
testMonster spriteManager Input{..} = do
  anim <- loadImage spriteManager "monsters/fatc/fatca.cl2"
  animLength <- getSpriteAnimLength anim
  frame <- accum (\acc d -> (acc + d) `mod` animLength) 0 (1 <$ inputTick)
  let pos = constDyn (P (V2 55 50))
  pure $ Monster anim (constant 100) frame pos never

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

hookLevelObjects :: (PerformEvent t m, MonadIO (Performable m), MonadIO m)
                 => [Monster t]
                 -> m LevelObjects
hookLevelObjects monsters = do
  levelObjects <- createLevelObjects
  traverse (updateObject levelObjects) monsters
  pure levelObjects
  where
    updateObject objs Monster{..} = do
      let posFrame = attachPromptlyDyn monsterPos (updated monsterAnimationFrame)
      performEvent_ $ (\(pos, frame) -> do spriteCacheIndex <- getSpriteCacheIndex monsterAnim
                                           updateLevelObject objs pos spriteCacheIndex frame) <$> posFrame

game :: SpriteManager -> Level -> SDLApp t m
game spriteManager level sel = do
  let keyPress = fmap (keysymKeycode . keyboardEventKeysym) .
                 ffilter ((== Pressed) . keyboardEventKeyMotion) .
                 select sel $
                 SDLKeyboard
      eQuit = void $ ffilter (== KeycodeEscape) keyPress
      tick = select sel SDLTick
  cameraPos <- screenScroll (P (V2 50 50)) keyPress
  monster <- testMonster spriteManager (Input tick)
  let monsters = [monster]
  let game' = Game cameraPos monsters
  levelObjects <- hookLevelObjects monsters
  performEvent_ $ renderGame spriteManager level levelObjects game' <$ tick
  performEvent_ $ liftIO quit <$ eQuit
  return eQuit

-- need to figure out correct monad stack, this looks tedious with all the liftIO
renderGame :: (Reflex t, MonadSample t m, MonadIO m)
           => SpriteManager
           -> Level
           -> LevelObjects
           -> Game t
           -> m ()
renderGame spriteManager level levelObjects Game{..} = do
  cameraPos@(P (V2 x y)) <- sample gameCameraPos
  renderFrame spriteManager level levelObjects cameraPos
