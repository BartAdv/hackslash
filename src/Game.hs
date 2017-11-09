{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Game where

import Data.List.NonEmpty (NonEmpty)
import Control.Monad (void, join, (<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import Data.Foldable (traverse_)
import Data.Function ((&), on)
import qualified Data.List as List
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, listToMaybe)
import Data.Monoid ((<>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import GHC.Word (Word32)
import Linear.V2
import Linear.Affine
import Reflex
import Reflex.SDL2 hiding (trace)
import SDL hiding (Renderer, Event, trace)
import System.Exit (exitSuccess)

import Animation
import Freeablo
import Path
import Types

import Debug.Trace hiding (traceEvent)
import Reflex.Dynamic (traceDyn)

ticksPerSecond :: Int
ticksPerSecond = 25

type MonsterID = Int

data Game t = Game {
  gameCameraPos :: Behavior t Coord,
  gameMonsters :: Dynamic t (Map MonsterID (Monster t))
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
  { inputTick :: Event t ()
  , inputKeyPress :: Event t Keycode
  , inputPositions :: Dynamic t (Map MonsterID Coord)
  , inputPushbacks :: Event t (Map MonsterID Coord)
  , inputLevel :: Level
  , inputCameraPos :: Behavior t Coord
  }

data Activity t = Activity
  { activityAnimation :: Animation
  , activityAnimationFrame :: Event t AnimationFrame
  , activityPosition :: Event t Coord
  , activityRotate :: Event t Direction
  , activityDone :: Event t ()
  }

walking :: (Reflex t, MonadHold t m, MonadFix m)
        => Input t
        -> Event t ()
        -> Event t Coord
        -> MonsterAnimSet
        -> Path
        -> m (Activity t)
walking Input{inputTick} start pushback animSet cmdPath = do
  -- freeablo wants it to be a percentage of the way towards next square
  moveDist <- foldDyn (\acc d -> (acc + d) `mod` 100) 0 $ 10 <$ inputTick -- 10 is derived from: secondsPerTick * 250 from freeablo
  let moved = void $ ffilter (== 0) (updated moveDist) -- `updated`, so that it doesn't fire at the very beginning
      rotated = leftmost [start, moved] -- change direction at the beginning and on every move
  -- on move, drop the coord from path. Direction is "one step ahead" of movement
  path <- accum (\p _ -> drop 1 p) cmdPath moved
  let ePath = tag path moved
      -- take next pos, unless pushback
      pos = leftmost [pushback, safeHeadE ePath]
      rotateDir = dirs $ tag path rotated
      done = leftmost [void pushback, void $ ffilter null ePath]
  frame <- foldDyn (\acc d -> (acc + d) `mod` animationLength animation) 0 $ 1 <$ inputTick
  let animFrame = AnimationFrame <$> frame <*> moveDist
  pure $ Activity animation (updated animFrame) pos rotateDir done
  where
    animation = animSetWalk animSet
    safeHeadE el = head <$> ffilter (not . null) el
    dirs el = (\l -> let (a:[b]) = take 2 l in getDir a b) <$> ffilter (\l -> length l >= 2) el

idling :: (Reflex t, MonadHold t m, MonadFix m)
       => Input t
       -> MonsterAnimSet
       -> m (Activity t)
idling Input{..} animSet = do
  let animation = animSetIdle animSet
  frame <- accum (\acc d -> (acc + d) `mod` animationLength animation) 0 $ 1 <$ inputTick
  let animFrame = (\f -> AnimationFrame f 0) <$> frame
  pure $ Activity animation animFrame never never never

data MonsterCmd = CmdIdle | CmdWalk Path deriving Show

findTarget :: Coord -> Map MonsterID Coord -> Maybe Coord
findTarget pos allPositions =
  allPositions
  & Map.toList
  & map snd
  & filter (\target -> target /= pos && distance' pos target < 10)
  & List.sortBy (comparing (distance' pos))
  & listToMaybe
  where
    distance' a b = distance (fromIntegral <$> a) (fromIntegral <$> b)

data MonsterState = MonsterState
 { monsterStatePosition :: Coord
 , monsterStateDirection :: Direction
 }

data MonsterConfig t = MonsterConfig
  { monsterConfigAnimSet :: MonsterAnimSet
  , monsterConfigInitialState :: MonsterState
  , monsterConfigActivity :: Dynamic t (Activity t)
  }

-- this, in fact should be called (or refactored more)  `gameObject`, or even `levelObject`
monster :: (Reflex t, MonadFix m, MonadHold t m)
        => MonsterConfig t
        -> m (Monster t)
monster MonsterConfig{..} = do
  let MonsterState{..} = monsterConfigInitialState
      -- looks strange with all those switches...
      actPos = switch (current $ activityPosition <$> monsterConfigActivity)
      actRotate = switchPromptlyDyn $ activityRotate <$> monsterConfigActivity
      actAnimFrame = switchPromptlyDyn (activityAnimationFrame <$> monsterConfigActivity)
  pos <- holdDyn monsterStatePosition actPos
  dir <- holdDyn monsterStateDirection actRotate
  frame <- holdDyn (AnimationFrame 0 0) actAnimFrame
  pure $ Monster (activityAnimation <$> monsterConfigActivity) frame pos dir never

testMonster :: (Reflex t, MonadFix m, MonadHold t m)
            => MonsterID -- currently used to pass information about collisions, is it good idea?
            -> MonsterAnimSet
            -> MonsterState
            -> Input t
            -> m (Monster t)
testMonster monsterID animSet initialState input@Input{..} = do
  initialActivity <- idling input animSet
  rec m <- monster (MonsterConfig animSet initialState activity)
      let pos = monsterPosition m
          done = switch (current $ activityDone <$> activity)
      activity <- holdDyn initialActivity (actions pos done)
  pure m
  where
    actions pos done =
      pushAlways (\case
                    CmdIdle -> idling input animSet
                    CmdWalk path -> walking input (void cmd) cmdPushBack animSet path) cmd
      where
      cmd = leftmost [ CmdIdle <$ done
                      , cmdApproach
                      , cmdGoTo
                      , cmdIdle
                      ]
      cmdIdle = CmdIdle <$ ffilter (== KeycodeI) inputKeyPress
      cmdApproach = let eTarget = fromJust <$> ffilter isJust (uncurry findTarget <$> attachPromptlyDyn pos (updated inputPositions))
                        ePath = init . fromJust <$> ffilter isJust (uncurry (findPath inputLevel) <$> attachPromptlyDyn pos eTarget)
                    in CmdWalk <$> ePath
      cmdGoTo = push (\_ -> do
                         pos <- sample (current pos)
                         cameraPos <- sample inputCameraPos
                         pure $ CmdWalk <$> findPath inputLevel pos cameraPos)
                     (ffilter (== KeycodeW) inputKeyPress)
      cmdPushBack = (fromJust . Map.lookup monsterID) <$> ffilter (Map.member monsterID) inputPushbacks

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

-- Hooks events that are necessary to update freeablo internal structures (`LevelObjects`)
-- that are used by renderer. Returns 'pushback' event, which is a workaround for
-- the limitation of `LevelObjects` that can only keep one object at given coord. This event
-- should be then used by game objects to be 'pushed back' in order to prevent from
-- occupying the same square.
hookMonsterMovement :: (PerformEvent t m, MonadSample t (Performable m), MonadIO (Performable m), MonadIO m)
                    => LevelObjects
                    -> Dynamic t (Map MonsterID (Monster t))
                    -> m (Event t (Map MonsterID Coord))
hookMonsterMovement levelObjects dMonsters = do
  let frameEvs = switchPromptlyDyn (mergeMap . fmap frameEv <$> dMonsters)
      moveEvs = switchPromptlyDyn (mergeMap . fmap moveEv <$> dMonsters)

  performEvent_ $ traverse_
    (\(Animation spriteGroup animLength, (pos, Direction dir, AnimationFrame frame dist)) -> do
        spriteCacheIndex <- getSpriteCacheIndex spriteGroup
        let to = followDir (Direction dir) pos
        updateLevelObject levelObjects pos spriteCacheIndex (frame + dir * animLength) to dist)
    <$> frameEvs

  -- collect move results
  moveRes <- performEvent $ traverse (\(from, to) -> do res <- moveLevelObject levelObjects from to
                                                        pure (from, res)) <$> moveEvs

  -- Get the moves that ended up in 'from' position - the 'pushbacks'
  pure $ ffilter (not . null) $ (Map.map snd . Map.filter (\(from, to) -> from == to)) <$> moveRes
  where
    -- extract event needed to update the LevelObject' frame
    frameEv Monster{..} =
      let posDirFrame = (,,) <$> monsterPosition <*> monsterDirection <*> monsterAnimationFrame
      in attach (current monsterAnim) (updated posDirFrame)
    -- extract event needed to move the LevelObject' on the grid
    moveEv Monster{monsterPosition} =
      -- attach current position to an event with updated position to obtain from -> to event
      attach (current monsterPosition) (updated monsterPosition)

game :: (ReflexSDL2 r t m, (MonadSample t (Performable m))) => SpriteManager -> Level -> m ()
game spriteManager level = do
  levelObjects <- createLevelObjects
  evKey <- getKeyboardEvent
  tick <- getDeltaTickEvent
  gameTick <- getRecurringTimerEventWithEventCode 0 (1000 `div` ticksPerSecond)

  let keyPress = (keysymKeycode . keyboardEventKeysym) <$> ffilter (\k -> keyboardEventKeyMotion k == Pressed) evKey
      eSpawn = void $ ffilter (== KeycodeS) keyPress
      eQuit = void $ ffilter (== KeycodeEscape) keyPress
  cameraPos <- screenScroll (P (V2 55 65)) keyPress


  fatc <- loadMonsterAnimSet spriteManager "fatc" "fatc"

  rec input <- pure $ Input gameTick keyPress positions pushbacks level cameraPos
      -- monster1 <- testMonster 0 fatc (MonsterState (P (V2 61 68)) DirSE) input
      let initialMonsters = Map.fromList [ -- (0, monster1)
                                         ]
      monsters <- traceDynWith (\ms -> "Monsters: " ++ show (Map.size ms)) <$>
                  foldDynM (\_ ms -> do
                               pos <- sample cameraPos
                               let monsterID = Map.size ms
                                   animSet = fatc
                               monster <- testMonster monsterID animSet (MonsterState pos DirN) input
                               pure $ Map.insert monsterID monster ms
                           )
                           initialMonsters
                           eSpawn
      let positions = monsterPositions monsters
      pushbacks <- hookMonsterMovement levelObjects monsters

  let game' = Game cameraPos monsters
  performEvent_ $ renderGame spriteManager level levelObjects game' <$ tick
  performEvent_ $ liftIO quitGame <$ eQuit

monsterPositions :: Reflex t
                 => Dynamic t (Map MonsterID (Monster t))
                 -> Dynamic t (Map MonsterID Coord)
monsterPositions monsters =
  joinDynThroughMap (Map.map monsterPosition <$> monsters)

renderGame :: (Reflex t, MonadSample t m, MonadIO m)
           => SpriteManager
           -> Level
           -> LevelObjects
           -> Game t
           -> m ()
renderGame spriteManager level levelObjects Game{..} = do
  cameraPos@(P (V2 x y)) <- sample gameCameraPos
  renderFrame spriteManager level levelObjects cameraPos

quitGame = do
  quit
  exitSuccess