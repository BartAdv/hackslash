{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  { inputTick :: Event t Word32
  , inputKeyPress :: Event t Keycode }

type Path = [Direction]

data Activity t = Activity
  { activityAnimation :: Animation
  , activityAnimationFrame :: Event t AnimationFrame
  , activityMove :: Event t Direction
  , activityRotate :: Event t Direction
  }

walking :: (Reflex t, MonadHold t m, MonadFix m)
        => Event t ()
        -> Input t
        -> MonsterAnimSet
        -> Path
        -> m (Activity t)
walking start Input{..} animSet cmdPath = do
  -- freeablo wants it to be a percentage of the way towards next square
  moveDist <- foldDyn (\acc d -> (acc + d) `mod` 100) 0 $ 10 <$ inputTick -- 10 is derived from: secondsPerTick * 250 from freeablo
  let moved = void $ ffilter (== 0) (updated moveDist)
      rotated = leftmost [start, moved] -- change direction at the beginning and on every move
  -- on move, drop the coord from path. Direction is "one step ahead" of movement
  path <- accum (\p _ -> drop 1 p) cmdPath moved
  dirs <- accum (\p _ -> drop 1 p) cmdPath rotated
  let moveDir = safeHeadE $ tag path moved
      rotateDir = safeHeadE $ tag dirs rotated
  frame <- foldDyn (\acc d -> (acc + d) `mod` animationLength animation) 0 $ 1 <$ inputTick
  let animFrame = AnimationFrame <$> frame <*> moveDist
  pure $ Activity animation (updated animFrame) moveDir rotateDir
  where
    animation = animSetWalk animSet
    safeHeadE el = head <$> ffilter (not . null) el

idling :: (Reflex t, MonadHold t m, MonadFix m)
       => Input t
       -> MonsterAnimSet
       -> m (Activity t)
idling Input{..} animSet = do
  let animation = animSetIdle animSet
  frame <- accum (\acc d -> (acc + d) `mod` animationLength animation) 0 $ 1 <$ inputTick
  let animFrame = (\f -> AnimationFrame f 0) <$> frame
  pure $ Activity animation animFrame never never

data MonsterCmd = CmdIdle | CmdWalk Path

testPath = join $ repeat [DirSE, DirSE,  DirS, DirS,  DirSW, DirSW, DirW, DirW, DirNW, DirNW, DirN, DirN, DirNE, DirNE, DirE, DirE]

actions :: (Reflex t)
        => Input t
        -> MonsterAnimSet
        -> Event t (Activity t)
actions input@Input{..} animSet =
  pushAlways (\case
                 CmdIdle -> idling input animSet
                 CmdWalk path -> walking (void cmd) input animSet path) cmd
  where
    cmd = leftmost [cmdWalk, cmdIdle]
    cmdIdle = CmdIdle <$ ffilter (== KeycodeI) inputKeyPress
    cmdWalk = CmdWalk testPath <$ ffilter (== KeycodeW) inputKeyPress

testMonster :: (Reflex t, MonadFix m, MonadHold t m, MonadIO m)
            => SpriteManager
            -> Input t
            -> m (Monster t)
testMonster spriteManager input@Input{..} = do
  animSet@MonsterAnimSet{..} <- loadMonsterAnimSet spriteManager "fatc"
  let initialPos = P (V2 61 68)
      initialDir = DirSE
  initialActivity <- idling input animSet
  action <- holdDyn initialActivity $ actions input animSet
  let move = switchPromptlyDyn (activityMove <$> action)
  pos <- foldDyn followDir initialPos move
  frame <- holdDyn (AnimationFrame 0 0) $ switchPromptlyDyn (activityAnimationFrame <$> action)
  dir <- holdDyn initialDir $ switchPromptlyDyn $ activityRotate <$> action
  pure $ Monster (activityAnimation <$> action) frame pos dir never

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
  monster <- testMonster spriteManager (Input tick keyPress)
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
