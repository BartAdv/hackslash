{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
module Graphics
       (PillarTexture(..)
       ,createPillarTexture) where

import Data.List (groupBy)
import Data.Maybe (isJust, fromJust)
import Data.Word (Word8)
import Data.Vector (Vector, (!))
import Data.ByteString (ByteString, pack)
import qualified Data.Vector as V
import Foreign.C.Types
import Linear.Affine
import Linear.V2
import SDL hiding (copy)

import Dat.Cel
import qualified Dat.Pal as Pal
import Dat.Min

framePixels :: Pal.Palette -> DecodedCel -> ByteString
framePixels palette DecodedCel{..} =
  -- flip it vertically
  let pitch = decodedCelWidth * 4
      rows = groupBy
               (\(i1,_) (i2,_) -> i1 `div` pitch == i2 `div` pitch)
               (zip [0..] (getColors decodedCelColors))
  in pack $ map snd $ concat $ reverse rows
  where
    getColors :: [CelColor] -> [Word8]
    getColors = concat . fmap (maybe [0, 0, 0, 0] (\c -> let (r,g,b) = Pal.getColor palette c in [0xff, b, g, r]))

data PillarTexture = PillarTexture
  { pillarTexture :: Texture
  , pillarTextureSize :: V2 CInt }

createPillarTexture :: Renderer
                    -> Pal.Palette
                    -> Vector DecodedCel
                    -> Pillar
                    -> IO PillarTexture
createPillarTexture renderer palette cels pillar = do
  let pillarBlocks = fmap fromJust $ V.filter isJust pillar
      height = V.length pillarBlocks `div` 2 * 32
      width  = 64
  tex <- createTexture renderer RGBA8888 TextureAccessStatic (fmap fromIntegral $ V2 width height)
  mapM_ (\(i, (frameNum, _)) -> do
            let cel = cels ! frameNum
                (x,y) = (i `mod` 2 * 32, i `div` 2 * 32)
                pixels = framePixels palette cel
            updateTexture tex (Just $ Rectangle (P (V2 x y)) (V2 32 32)) pixels (32 * 4)) $ zip [0..] (V.toList pillarBlocks)
  return $ PillarTexture tex (fmap fromIntegral $ V2 width height)
