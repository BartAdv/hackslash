-- https://github.com/doggan/diablo-file-formats
module Dat.Sol
       (SolFile(..)
       ,SolFlag
       ,flagCollision
       ,flagCollisionRange
       ,flagTransparency
       ,isFlag) where

import Dat.Utils

data SolFile = SolFile { solName :: String, solData :: ByteString }

newtype SolFlag = SolFlag Word8

flagCollision :: SolFlag
flagCollision = SolFlag 0x01
flagCollisionRange :: SolFlag
flagCollisionRange = SolFlag 0x04
flagTransparency :: SolFlag
flagTransparency = SolFlag 0x08

isFlag :: SolFlag -> SolFile -> Int -> Bool
isFlag (SolFlag f) file idx = pillar .&. f == 1
  where pillar = index (solData file) idx
