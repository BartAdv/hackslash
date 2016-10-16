{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module Test where

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
import Rendering

path :: FilePath
path = "diabdat/plrgfx/warrior"


test :: IO ()
test = do
  buffer <- BS.readFile (path </> "wha/whaas.cl2")
  case loadCl2 "whaas" buffer of
    Left msg -> putStrLn msg
    Right cels -> print $ V.head cels
