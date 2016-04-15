-- https://github.com/doggan/diablo-file-formats
module Dat.Til
       (Til
       ,loadTil) where

import Control.Monad (replicateM)
import Data.Binary.Strict.Get (isEmpty, getWord16le)
import Data.Vector (fromList)
import Dat.Utils

type Til = Vector TilIndex
type TilIndex = Int

loadTil :: ByteString -> Either String (Vector Til)
loadTil buffer =
  let (res, _) = runGet readSquares buffer
  in fmap fromList res
  where
    readSquares = do
      squares <- fmap fromList $ replicateM 4 (fmap fromIntegral getWord16le)
      empty <- isEmpty
      fmap (squares:) (if empty then return [] else readSquares)
