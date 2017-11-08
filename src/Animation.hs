module Animation where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Freeablo

data Animation = Animation
  { animationSpriteGroup :: SpriteGroup
  , animationLength :: Int }

data MonsterAnimSet = MonsterAnimSet
  { animSetIdle :: Animation
  , animSetWalk :: Animation
  , animSetAttack :: Animation
  , animSetDie :: Animation }

-- loadMonsterAnimSet :: MonadIO m => SpriteManager -> String -> String -> m MonsterAnimSet
-- loadMonsterAnimSet spriteMgr dir name = do
--   idle <- load "n"
--   walk <- load "w"
--   att  <- load "h"
--   die  <- load "d"
--   pure $ MonsterAnimSet idle walk att die
--   where
--     path suffix = "monsters/" ++ dir ++ "/" ++ name ++ suffix ++ ".cl2"
--     load suffix = do
--       sg <- loadImage spriteMgr $ path suffix
--       len <- getSpriteAnimLength sg
--       pure $ Animation sg len
