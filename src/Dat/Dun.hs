-- https://github.com/doggan/diablo-file-formats
module Dat.Dun
       (Dun(..)
       ,load) where

import Control.Monad (replicateM)
import Data.Binary.Strict.Get (getWord16le)
import Data.Word (Word16)
import Data.Vector ((!), fromList)
import Dat.Til (Til)
import Linear.Affine
import Linear.V2

import Dat.Utils
import Dat.Min (Pillar)

data Dun = Dun { dunColCount :: Int
               , dunRowCount :: Int
               , dunPillars :: Vector (Maybe Pillar)
               , dunStartCoords :: Point V2 Int }

load :: String -> Vector Til -> Vector Pillar -> ByteString -> Either String Dun
load dunName tils pillars buffer = do
  (colCount, rowCount, rawPillarData) <- fst $ runGet readRawPillarData buffer
  let unpacked = unpackTils colCount rowCount (fromList rawPillarData)
      pillars' = fmap (fmap (pillars !)) unpacked
  return $ Dun (colCount * 2) (rowCount * 2) pillars' startCoords
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
    startCoords = getStartCoord dunName

getStartCoord :: String -> Point V2 Int
getStartCoord "sector1s" = P $ V2 46 46
getStartCoord "sector2s" = P $ V2 46 0
getStartCoord "sector3s" = P $ V2 0 46
getStartCoord "sector4s" = P $ V2 0 0
getStartCoord _ = undefined
