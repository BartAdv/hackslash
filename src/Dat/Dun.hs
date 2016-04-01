-- https://github.com/doggan/diablo-file-formats
module Dat.Dun
       (load) where

import Control.Monad (replicateM)
import Data.Binary.Strict.Get (getWord16le)
import Data.Word (Word16)
import Data.Vector ((!), fromList)
import Dat.Til (Til)

import Dat.Utils

load :: String -> Vector Til -> ByteString -> Either String (Int, Int, Vector (Maybe Int))
load dunName tils buffer = do
  (colCount, rowCount, rawPillarData) <- fst $ runGet readRawPillarData buffer
  let unpacked = unpackTils colCount rowCount (fromList rawPillarData)
  return (colCount * 2, rowCount * 2, unpacked)
  where
    unpackTils :: Int -> Int -> Vector Word16 -> Vector (Maybe Int)
    unpackTils colCount rowCount rawPillarData =
      fromList $ fmap parsePillarData [(row, col) | row <- [0..rowCount*2 - 2], col <- [0..colCount*2 - 2]]
      where parsePillarData (row, col) =
              let squareIndexPlus1 = rawPillarData ! (row*colCount `div` 2 + col `div` 2)
              in if squareIndexPlus1 /= 0
                 then let tilIdx = row `mod` 2 * 2 + col `mod` 2
                          til = tils ! (fromIntegral squareIndexPlus1 - 1)
                      in Just $ til ! tilIdx
                 else Nothing
    readRawPillarData = do
      colCount <- getWord16le
      rowCount <- getWord16le
      rawData <- replicateM (fromIntegral rowCount * fromIntegral colCount) getWord16le
      return (fromIntegral colCount, fromIntegral rowCount, rawData)

    -- // levels/towndata/sector1s.dun -> sector1s
    -- var dunName = path.basename(dunPath, '.dun');
    -- var startCoord = getStartCoord(dunName);
