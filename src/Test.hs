{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module Test where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Word (Word8)
import qualified Data.Vector as V
import Foreign.Store
import Linear.Affine
import Linear.V2
import qualified SDL
import System.FilePath.Posix ((</>))

import Assets
import Dat.Cel
import qualified Dat.Pal as Pal
import Graphics
import Rendering

path :: FilePath
path = "diabdat"

test :: IO ()
test = do
  (window, renderer) <- initializeSDL
  tex <- loadAnimation (path </> "plrgfx/warrior/wha/whaas.cl2") renderer
  SDL.clear renderer
  SDL.copy renderer tex (Just $ SDL.Rectangle (P (V2 0 0)) (V2 96 96)) (Just $ SDL.Rectangle (P (V2 0 0)) (V2 96 96))
  SDL.present renderer
  _ <- getLine
  finalizeSDL (window, renderer)
  return ()
