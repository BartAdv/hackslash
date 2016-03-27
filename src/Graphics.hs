{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
module Graphics
       (celSurfaces) where

import Prelude
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.String (newCString)
import SDL hiding (copy)
import SDL.Raw.Video (saveBMP)
import Linear.V2 (V2(..))

import Dat.Cel
import qualified Dat.Pal as Pal

test :: IO ()
test = do
  pal <- BS.readFile "diabdat/levels/towndata/town.pal"
  townCel <- BS.readFile "diabdat/levels/towndata/town.cel"
  let (Right townCels) = loadCel townCel "town"
  print $ length townCels
  surfaces <- celSurfaces [townCels !! 1280] pal
  let (Surface ptr _) = head surfaces
  filename <- newCString "test.bmp"
  _ <- saveBMP ptr filename
  return ()


celSurfaces :: [DecodedCel] -> Pal.Palette -> IO [Surface]
celSurfaces frames palette = mapM frameSurface frames
  where
    frameSurface :: DecodedCel -> IO Surface
    frameSurface DecodedCel{..} = do
      let pitch = decodedCelWidth * 4
      pixels <- MV.new $ decodedCelWidth * decodedCelHeight * 4
      -- flip it vertically
      mapM_ (\(i,c) -> let px = (decodedCelHeight-1 - i `div` pitch) * pitch + i `mod` pitch
                       in MV.write pixels px c)
            (zip [0..] (getColors decodedCelColors))
      createRGBSurfaceFrom
                   pixels
                   (fmap fromIntegral (V2 decodedCelWidth decodedCelHeight))
                   (fromIntegral pitch)
                   RGBA8888
    getColors :: [CelColor] -> [Word8]
    getColors = concat . fmap (maybe [0, 0, 0, 0] (\c -> let (r,g,b) = Pal.getColor palette c in [0xff, b, g, r]))
