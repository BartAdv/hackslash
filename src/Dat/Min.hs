-- https://github.com/doggan/diablo-file-formats
module Dat.Min
       (Pillar
       ,PillarBlock
       ,FrameNum
       ,PillarType
       ,loadMin
       ,getBlockCountPerPillar) where

import Control.Monad (replicateM)
import Data.Binary.Strict.Get (isEmpty, getWord16le)
import Data.Word (Word16)
import Data.Vector (fromList)
import Dat.Utils

type Pillar = Vector PillarBlock
type PillarBlock = Maybe (FrameNum, PillarType)
type FrameNum = Int
type PillarType = Word16

loadMin :: String -> ByteString -> Either String (Vector Pillar)
loadMin minName buffer =
  let (res, _) = runGet readPillars buffer
  in fmap (fromList . fmap fromList) res
  where
    readPillars = do
      blocks <- replicateM blockCountPerPillar readPillarBlock
      empty <- isEmpty
      fmap (blocks:) (if empty then return [] else readPillars)
    readPillarBlock = do
      rawBlockValue <- getWord16le
      let frameNumPlus1 = rawBlockValue .&. 0x0FFF
      if frameNumPlus1 /= 0
        then return $ Just (fromIntegral $ frameNumPlus1 - 1, (rawBlockValue .&. 0x7000) `shiftR` 12)
        else return Nothing
    blockCountPerPillar = getBlockCountPerPillar minName

getBlockCountPerPillar :: String -> Int
getBlockCountPerPillar "l1" = 10
getBlockCountPerPillar "l2" = 10
getBlockCountPerPillar "l3" = 10
getBlockCountPerPillar "l4" = 16
getBlockCountPerPillar "town" = 16
getBlockCountPerPillar _ = undefined
