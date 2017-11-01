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
{-# LANGUAGE ViewPatterns #-}
module Game where

import Data.List.NonEmpty (NonEmpty)
import Control.Monad (void, join, (<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import Data.Foldable (traverse_)
import Data.Function ((&))
import qualified Data.List as List
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe)
import Data.Monoid ((<>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import GHC.Word (Word32)
import Linear.V2
import Linear.Affine
import Reflex
import Reflex.SDL.Event
import Reflex.SDL.Host
import SDL hiding (Renderer, Event, trace)

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
  { inputTick :: Event t Word32
  , inputKeyPress :: Event t Keycode
  , inputPositions :: Dynamic t (Map MonsterID Coord)
  , inputLevel :: Level
  , inputCameraPos :: Behavior t Coord
  }

data Activity t = Activity
  { activityAnimation :: Animation
  , activityAnimationFrame :: Event t AnimationFrame
  , activityPosition :: Event t Coord
  , activityRotate :: Event t Direction
  }

walking :: (Reflex t, MonadHold t m, MonadFix m)
        => Event t ()
        -> Input t
        -> MonsterAnimSet
        -> Path
        -> m (Activity t)
walking start Input{inputTick} animSet cmdPath = do
  -- freeablo wants it to be a percentage of the way towards next square
  moveDist <- foldDyn (\acc d -> (acc + d) `mod` 100) 0 $ 10 <$ inputTick -- 10 is derived from: secondsPerTick * 250 from freeablo
  let moved = void $ ffilter (== 0) (updated moveDist)
      rotated = leftmost [start, moved] -- change direction at the beginning and on every move
  -- on move, drop the coord from path. Direction is "one step ahead" of movement
  path <- accum (\p _ -> drop 1 p) cmdPath moved
  let pos = safeHeadE $ tag path moved
      rotateDir = dirs $ tag path rotated
  frame <- foldDyn (\acc d -> (acc + d) `mod` animationLength animation) 0 $ 1 <$ inputTick
  let animFrame = AnimationFrame <$> frame <*> moveDist
  pure $ Activity animation (updated animFrame) pos rotateDir
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
  pure $ Activity animation animFrame never never

data MonsterCmd = CmdIdle | CmdWalk Coord deriving Show

actions :: Reflex t
        => Input t
        -> Dynamic t Coord
        -> MonsterAnimSet
        -> Event t (Activity t)
actions input@Input{..} pos animSet =
  pushAlways (\case
                 CmdIdle -> idling input animSet
                 CmdWalk target -> do
                   let path = fmap (\pos -> fromMaybe [] $ findPath inputLevel pos target) pos
                   path' <- sample (current path)
                   walking (void cmd) input animSet path') cmd
  where
    cmd = leftmost [ cmdWalk
                   , cmdGoTo
                   , cmdIdle
                   ]
    cmdIdle = CmdIdle <$ ffilter (== KeycodeI) inputKeyPress
    eTarget = updated $ findTarget <$> pos <*> inputPositions
    cmdWalk = CmdWalk . fromJust <$> ffilter isJust eTarget
    cmdGoTo = traceEvent "goto" $ pushAlways (\_ -> CmdWalk <$> sample inputCameraPos) $ ffilter (== KeycodeW) inputKeyPress

findTarget :: Coord -> Map MonsterID Coord -> Maybe Coord
findTarget pos allMoves =
  allMoves
  & Map.toList
  & map snd
  & filter (\target -> target /= pos && distance' pos target < 5)
  & List.sortBy (comparing (distance' pos))
  & listToMaybe
  where
    distance' a b = distance (fromIntegral <$> a) (fromIntegral <$> b)

data MonsterState = MonsterState
 { monsterStatePosition :: Coord
 , monsterStateDirection :: Direction
 }

testMonster :: (Reflex t, MonadFix m, MonadHold t m)
            => MonsterAnimSet
            -> MonsterState
            -> Input t
            -> m (Monster t)
testMonster animSet MonsterState{..} input = do
  initialActivity <- idling input animSet

  rec action <- holdDyn initialActivity $ actions input pos animSet
      pos <- holdDyn monsterStatePosition $ switch (current $ activityPosition <$> action)
  dir <- holdDyn monsterStateDirection $ switchPromptlyDyn $ activityRotate <$> action
  frame <- holdDyn (AnimationFrame 0 0) $ switchPromptlyDyn (activityAnimationFrame <$> action)
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
  performEvent_ $ traverse_ (uncurry (moveLevelObject levelObjects)) <$> moveEvs
  pure $ fmap snd <$> moveEvs
  where
    -- extract event needed to update the LevelObject' frame
    frameEv Monster{..} =
      let posDirFrame = (,,) <$> monsterPosition <*> monsterDirection <*> monsterAnimationFrame
      in attach (current monsterAnim) (updated posDirFrame)
    -- extract event needed to move the LevelObject' on the grid
    moveEv Monster{monsterPosition} =
      -- attach current position to an event with updated position to obtain from -> to event
      attach (current monsterPosition) (updated monsterPosition)

game :: SpriteManager -> Level -> SDLApp t m
game spriteManager level sel = do
  levelObjects <- createLevelObjects
  let keyPress = fmap (keysymKeycode . keyboardEventKeysym) .
                 ffilter ((== Pressed) . keyboardEventKeyMotion) .
                 select sel $
                 SDLKeyboard
      eQuit = void $ ffilter (== KeycodeEscape) keyPress
      eSpawn = void $ ffilter (== KeycodeS) keyPress
      tick = select sel SDLTick
  cameraPos <- screenScroll (P (V2 55 65)) keyPress

  fatc <- loadMonsterAnimSet spriteManager "fatc"

  rec input <- pure $ Input tick keyPress positions level cameraPos
      monster1 <- testMonster fatc (MonsterState (P (V2 61 68)) DirSE) input
      let initialMonsters = Map.fromList [ (0, monster1)
                                         ]
      monsters <- foldDynM (\_ ms -> do
                               pos <- sample cameraPos
                               monster <- testMonster fatc (MonsterState pos DirN) input
                               pure $ Map.insert (Map.size ms) monster ms
                           )
                           initialMonsters
                           eSpawn
      moves <- hookMonsterMovement levelObjects monsters
      positions <- monsterPositions monsters moves

  let game' = Game cameraPos monsters
  performEvent_ $ renderGame spriteManager level levelObjects game' <$ tick
  performEvent_ $ liftIO quit <$ eQuit
  return eQuit

-- Calculate Dynamic with monster positions, given the monster move events
-- and spawn events (so that newly spawned monsters are visible to others).
monsterPositions :: (Reflex t, MonadHold t m, MonadFix m)
                 => Dynamic t (Map MonsterID (Monster t))
                 -> Event t (Map MonsterID Coord)
                 -> m (Dynamic t (Map MonsterID Coord))
monsterPositions monsters moves = do
    -- flip, so that difference works from updated
    let eSpawned = (uncurry . flip) Map.difference <$> attach (current monsters) (updated monsters)
    spawnedPositions <-
      foldDynM (\spawnedMonsters _ ->
                  Map.traverseWithKey (\_ m -> sample $ current $ monsterPosition m)
                                      spawnedMonsters)
                Map.empty
                eSpawned
    dMoves <- holdDyn Map.empty moves
    pure $ Map.union <$> dMoves <*> spawnedPositions

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
