{-# LANGUAGE RecordWildCards #-}
module Assets (Assets(..), loadAssets) where

import SDL (Renderer)

import Level

data Assets = Assets
  { assetsLevel :: Level
  }

loadAssets :: Renderer -> FilePath -> String -> IO Assets
loadAssets renderer path levelName = do
  level <- loadTown renderer path -- yeah, I know
  return $ Assets level

