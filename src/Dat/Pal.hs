-- https://github.com/doggan/diablo-file-formats
module Dat.Pal
       (Palette
       ,Color
       ,getColor) where

import Dat.Utils

type Palette = ByteString
type Color = (Word8, Word8, Word8) -- RGB

getColor :: Palette -> Word8 -> Color
getColor buffer idx = (col 0, col 1, col 2)
  where col i = index buffer (fromIntegral idx*3 + i)

