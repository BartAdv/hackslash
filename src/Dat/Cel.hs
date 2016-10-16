{-# LANGUAGE OverloadedLists #-}

-- https://github.com/doggan/diablo-file-formats

module Dat.Cel
       (module Dat.CelDecode, loadCel, loadCl2) where

import Control.Monad (replicateM, foldM)
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
celConfig "whaas"  = (96,96,10)
celConfig _ = (32,32,0)

loadCel :: String -> ByteString -> Either String (Vector DecodedCel)
loadCel celName buffer =
  let (_,_,headerSize) = celConfig celName
      (frames, _) = runGet (readFrames headerSize buffer) buffer
  in fmap (fromList . map (decodeCelFrame celName)) frames

archiveImageCount = 8
archiveImageCount :: Int

loadCl2 :: String -> ByteString -> Either String (Vector DecodedCel)
loadCl2 cl2Name buffer =
  case BS.head buffer of
    32 -> let (frames, _) = runGet readArchiveFile buffer
          in fmap (fromList . map (decodeCl2Frame cl2Name)) frames
    _ -> Left  "Not an archive file!!"
  where
    readArchiveFile = do
      headerOffsets <- replicateM archiveImageCount (fromIntegral <$> getWord32le)
      let (_,_,headerSize) = celConfig cl2Name
      foldM (\frames offsetA ->
               (frames ++) <$> readFrames headerSize (BS.drop offsetA buffer))
        [] headerOffsets

-- TODO: figure out if can get away without passing buffer as arg
-- and use functionality of Data.Binary to skip to offsets
readFrames :: Int -> ByteString -> Get [(Int, ByteString)]
readFrames headerSize buffer = do
  frameCount <- fmap fromIntegral getWord32le
  frameOffsets <- replicateM (frameCount + 1) (fmap fromIntegral getWord32le)
  return $ (readFrame headerSize) <$> zip3 [0..] frameOffsets (drop 1 frameOffsets)
   where
     readFrame :: Int -> (Int,Int,Int) -> (Int, ByteString)
     readFrame headerSize (frameNum, offsetA, offsetB) =
       let frameStart = offsetA + headerSize
           frameEnd = offsetB
           frameSize = frameEnd - frameStart
       in (frameNum, BS.take frameSize . BS.drop frameStart $ buffer)

decodeCelFrame :: String -> (Int, ByteString) -> DecodedCel
decodeCelFrame celName (frameNum, frameData) = frameDecoder frameData w h
  where (w,h,_) = celConfig celName
        frameDecoder = getCelFrameDecoder celName frameData frameNum

decodeCl2Frame :: String -> (Int, ByteString) -> DecodedCel
decodeCl2Frame celName (_frameNum, frameData) = decodeFrameType6 frameData w h
  where (w,h,_) = celConfig celName
