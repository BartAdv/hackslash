{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
module Graphics
       (PillarTexture(..)
       ,createPillarTexture) where

import Control.Monad (when)
import Data.List (groupBy)
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Word (Word8)
import Data.Vector (Vector, (!))
import Data.ByteString (ByteString, pack)
import qualified Data.Vector as V
import Foreign.C.Types
import Linear.Affine (Point(..))
import Linear.V2
import SDL hiding (copy)

import Dat.Cel
import qualified Dat.Pal as Pal
import Dat.Min

framePixels :: Pal.Palette -> DecodedCel -> ByteString
framePixels palette DecodedCel{..} =
  -- flip it vertically
  let reversedRows = [decodedCelColors ! ((decodedCelHeight - y) * decodedCelHeight + (x-1))  | y <- [1..decodedCelHeight], x <- [1..decodedCelWidth]]
  in pack $ concatMap getColor reversedRows
  where
    getColor :: CelColor -> [Word8]
    getColor = (maybe [0, 0, 0, 0] (\c -> let (r,g,b) = Pal.getColor palette c in [0xff, b, g, r]))

data PillarTexture = PillarTexture
  { pillarTexture :: Texture
  , pillarTextureSize :: V2 CInt }

createPillarTexture :: Renderer
                    -> Pal.Palette
                    -> Vector DecodedCel
                    -> Pillar
                    -> IO PillarTexture
createPillarTexture renderer palette cels pillar = do
  let pillarBlocks = removeEmpty (V.toList pillar)
      height = max (length pillarBlocks `div` 2 * 32) 1 -- don't know why some pillars are empty, and 0 size is unacceptable
      width  = 64
  tex <- createTexture renderer RGBA8888 TextureAccessStatic (fromIntegral <$> V2 width height)
  SDL.textureBlendMode tex $= SDL.BlendAlphaBlend
  mapM_ (\(i, pillarBlock) -> when (isJust pillarBlock) $ do
            let Just (frameNum, _) = pillarBlock
                cel = cels ! frameNum
                (x,y) = (i `mod` 2 * 32, i `div` 2 * 32)
                pixels = framePixels palette cel
            updateTexture tex (Just $ Rectangle (P (V2 x y)) (V2 32 32)) pixels (32 * 4)) $ zip [0..] pillarBlocks
  return $ PillarTexture tex (fromIntegral <$> V2 width height)
  where
    -- remove empty pairs of blocks to not waste texture space
    removeEmpty (Nothing:Nothing:bs) = removeEmpty bs
    removeEmpty bs = bs
