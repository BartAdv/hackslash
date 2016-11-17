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
