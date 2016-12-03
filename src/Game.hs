{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Game where

import Control.Monad (void, join, (<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import GHC.Word (Word32)
import Linear.V2
import Linear.Affine
import Reflex
import Reflex.SDL.Event
import Reflex.SDL.Host
import SDL hiding (Renderer, Event, trace)

import Animation
import Freeablo
import Types

import Debug.Trace hiding (traceEvent)
import Reflex.Dynamic (traceDyn)

ticksPerSecond :: Int
ticksPerSecond = 25

data Game t = Game {
  gameCameraPos :: Behavior t Coord,
  gameMonsters :: [Monster t]
}

type Health = Int

data AnimationFrame = AnimationFrame
  { animationFrameIdx :: Int
  , animationFrameMoveDist :: MoveDist }

data Monster t = Monster
  { monsterAnim :: Dynamic t Animation
  , monsterAnimationFrame :: Dynamic t AnimationFrame
  , monsterPosition :: Dynamic t Coord
  , monsterDirection :: Dynamic t Direction
  , monsterDie :: Event t () }

data Input t = Input
  { inputTick :: Event t Word32 }

type Path = [Coord]

data Movement t = Movement
  { movementDirection :: Dynamic t Direction
  , movementPosition :: Dynamic t Coord
  , movementAnimationFrame :: Dynamic t AnimationFrame
  }

movement :: (Reflex t, MonadHold t m, MonadFix m)
         => Input t
         -> (Coord, Direction)
         -> Animation
         -> Event t [Direction]
         -> m (Movement t)
movement Input{..} (initialPos, initialDir) Animation{animationLength} ePath = do
  -- freeablo wants it to be a percentage of the way towards next square
  moveDist <- accum (\acc d -> (acc + d) `mod` 100) 0 $ 10 <$ inputTick -- 10 is derived from: secondsPerTick * 250 from freeablo
  let moved = ffilter (== 0) (updated moveDist)
  path <- accum (flip ($)) [] $ leftmost [ const <$> ePath -- new path arrives, replace the coords
                                         , drop 1 <$ moved -- on move, drop the coord
                                         ]
  -- dir change either on new path or on next path fragment
  let dirChange = leftmost [void moved, void ePath]
  dir <- holdDyn initialDir $ head <$> ffilter (not . null) (tagPromptlyDyn path dirChange)
  let frameTick = inputTick
  frame <- accum (\acc d -> (acc + d) `mod` animationLength) 0 $ 1 <$ frameTick
  -- on move, use previous direction to calculate the coord
  let (fmap fst -> dirChange) = attach (current dir) (updated dir)
  pos <- foldDyn followDir initialPos dirChange
  let animFrame = AnimationFrame <$> frame <*> moveDist
  pure $ Movement dir pos animFrame

testMonster :: (Reflex t, MonadFix m, MonadHold t m, MonadIO m)
            => SpriteManager
            -> Input t
            -> m (Monster t)
testMonster spriteManager input@Input{..} = do
  animSet <- loadMonsterAnimSet spriteManager "fatc"
  let initialCoord = (P (V2 61 68))
      testPath = join $ repeat [DirSE, DirSE,  DirS, DirS,  DirSW, DirSW, DirW, DirW, DirNW, DirNW, DirN, DirN, DirNE, DirNE, DirE, DirE]
  testPath <- headE $ testPath <$ inputTick
  Movement{..} <- movement input (initialCoord, DirN) (animSetWalk animSet) testPath
  pure $ Monster (constDyn (animSetWalk animSet)) movementAnimationFrame movementPosition movementDirection never

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

hookLevelObjects :: (PerformEvent t m, MonadSample t (Performable m), MonadIO (Performable m), MonadIO m)
                 => [Monster t]
                 -> m LevelObjects
hookLevelObjects monsters = do
  levelObjects <- createLevelObjects
  traverse (updateObject levelObjects) monsters
  pure levelObjects
  where
    updateObject objs Monster{..} = do
      let bPos = current monsterPosition -- so that we refer to position before update
          bAnim = current monsterAnim
          animUpdate = (,) <$> monsterDirection <*> monsterAnimationFrame
          posFrame = attach bPos (updated animUpdate)
      performEvent_ $
        (\(pos, (Direction dir, AnimationFrame frame dist)) -> do
            Animation spriteGroup animLength <- sample bAnim
            spriteCacheIndex <- getSpriteCacheIndex spriteGroup
            let to = followDir (Direction dir) pos
            updateLevelObject objs pos spriteCacheIndex (frame + dir * animLength) to dist)
        <$> posFrame
      let fromTo = attach bPos (updated monsterPosition)
      performEvent_ $ uncurry (moveLevelObject objs) <$> fromTo

game :: SpriteManager -> Level -> SDLApp t m
game spriteManager level sel = do
  let keyPress = fmap (keysymKeycode . keyboardEventKeysym) .
                 ffilter ((== Pressed) . keyboardEventKeyMotion) .
                 select sel $
                 SDLKeyboard
      eQuit = void $ ffilter (== KeycodeEscape) keyPress
      tick = select sel SDLTick
  cameraPos <- screenScroll (P (V2 55 65)) keyPress
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
