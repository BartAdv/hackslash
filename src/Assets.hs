{-# LANGUAGE RecordWildCards #-}
module Assets ( Assets(..)
              , loadAssets
              , loadAnimation) where

import qualified Data.ByteString as BS
import SDL (Renderer, Texture)
import System.FilePath.Posix ((</>), (<.>))

import Dat
import Graphics
import Level

data Assets = Assets
  { assetsLevel :: Level
  , assetsTest :: Cl2Anim
  }

loadAnimation :: FilePath -> Palette -> Renderer -> IO Cl2Anim
loadAnimation filePath pal renderer = do
  buffer <- BS.readFile filePath
  case loadCl2 "whaas" buffer of
    Left msg -> error msg
    Right cels -> createCl2Anim renderer pal cels

loadAssets :: FilePath -> Renderer -> String -> IO Assets
loadAssets path renderer levelName = do
  pal <- BS.readFile "diabdat/levels/towndata/town.pal"
  level <- loadTown path renderer -- yeah, I know
  test <- loadAnimation (path </> "plrgfx/warrior/wha/whaas.cl2") pal renderer
  return $ Assets level test
