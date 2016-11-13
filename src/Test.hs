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
  pal <- BS.readFile "diabdat/levels/towndata/town.pal"
  anim <- loadAnimation (path </> "plrgfx/warrior/wha/whaas.cl2") pal renderer
  SDL.clear renderer
  renderAnimation renderer anim (P (V2 50 50)) 88
  SDL.present renderer
  _ <- getLine
  finalizeSDL (window, renderer)
  return ()
