-- https://github.com/doggan/diablo-file-formats

module Dat.Cel
       (module Dat.CelDecode, loadCel) where

import Control.Monad (replicateM)
import Data.Binary.Strict.Get
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Vector (Vector, fromList)

import Dat.CelDecode

celConfig :: String -> (Int,Int,Int)
celConfig "strytell" = (96,96,10)
celConfig "chest1" = (96,96,10)
celConfig "chest2" = (96,96,10)
celConfig "chest3" = (96,96,10)
celConfig _ = (32,32,0)

loadCel :: String -> ByteString -> Either String (Vector DecodedCel)
loadCel celName buffer =
  let (res, _) = runGet readData buffer in fmap fromList res where
  readData = do
    frameCount <- fmap fromIntegral getWord32le
    frameOffsets <- replicateM (frameCount + 1) (fmap fromIntegral getWord32le)
    let (_,_,headerSize) = celConfig celName
    return $ (decodeFrame . readFrame headerSize) <$> zip3 [0..] frameOffsets (drop 1 frameOffsets)
  readFrame :: Int -> (Int,Int,Int) -> (Int, ByteString)
  readFrame headerSize (frameNum, offsetA, offsetB) =
    let frameStart = offsetA + headerSize
        frameEnd = offsetB
        frameSize = frameEnd - frameStart
    in (frameNum, BS.take frameSize . BS.drop frameStart $ buffer)
  decodeFrame :: (Int, ByteString) -> DecodedCel
  decodeFrame (frameNum, frameData) = frameDecoder frameData w h
    where (w,h,_) = celConfig celName
          frameDecoder = getCelFrameDecoder celName frameData frameNum
