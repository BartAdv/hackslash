{-# LANGUAGE RecordWildCards #-}
module Assets (Assets(..), loadAssets, loadAnimation) where

import qualified Data.ByteString as BS
import SDL (Renderer, Texture)
import System.FilePath.Posix ((</>), (<.>))

import Dat
import Graphics
import Level

data Assets = Assets
  { assetsLevel :: Level
  , assetsTest :: Texture
  }

loadAnimation :: FilePath -> Renderer -> IO Texture
loadAnimation filePath renderer = do
  buffer <- BS.readFile filePath
  pal <- BS.readFile "diabdat/levels/towndata/town.pal"
  case loadCl2 "whaas" buffer of
    Left msg -> error msg
    Right cels -> createCl2Texture renderer pal cels

loadAssets :: FilePath -> Renderer -> String -> IO Assets
loadAssets path renderer levelName = do
  level <- loadTown path renderer -- yeah, I know
  test <- loadAnimation path renderer
  return $ Assets level undefined

