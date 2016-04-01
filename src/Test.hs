import Control.Monad
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Vector (Vector, fromList, toList, (!))
import Foreign.C.String (newCString)
import Linear.Affine
import Linear.V2
import SDL hiding (copy)
import SDL.Raw.Video (saveBMP)
import System.FilePath.Posix ((</>))

import Dat.Cel
import qualified Dat.Dun as Dun
import qualified Dat.Min as Min
import qualified Dat.Til as Til

import Graphics

path :: FilePath
path = "diabdat/levels/towndata/"

testDun = do
  tils <- BS.readFile (path </> "town.til") >>= return . Til.load
  Right (colCount, rowCount, dun) <- BS.readFile (path </> "sector1s.dun") >>= return . Dun.load "sector1s" tils
  mins <- BS.readFile (path </> "town.min") >>= return . Min.load "town"
  let pillars = fmap (pillar mins) dun
  return (colCount, rowCount, pillars)
  where
    pillar :: Vector Min.Pillar -> Maybe Int -> Maybe Min.Pillar
    pillar mins (Just minIdx) = Just $ mins ! minIdx
    pillar _ Nothing = Nothing

--

frames :: [Maybe Int]
frames = [Nothing,Nothing,Nothing,Just 1181,Nothing,Just 1182,Nothing,Just 1183,Just 1184,Just 1185,Just 1186,Just 1187,Just 1188,Just 1189,Just 1190,Just 1191]

pillarFrames :: Min.Pillar -> [Maybe Int]
pillarFrames pillar =
  map (fmap (fromIntegral . fst)) $ toList pillar

testCels :: [Maybe Int] -> IO ()
testCels frames = do
  pal <- BS.readFile "diabdat/levels/towndata/town.pal"
  townCel <- BS.readFile "diabdat/levels/towndata/town.cel"
  let (Right townCels) = fmap fromList $ loadCel townCel "town"
  print $ length townCels
  surfaces <- mapM (maybe (return Nothing) (\frame -> fmap (Just . head) $ celSurfaces [townCels ! frame] pal)) frames
  bmpSurface <- createRGBSurface (V2 64 16*32) RGBA8888
  mapM_ (\(i, s) -> let (x, y) = (i `mod` 2 * 32, i `div` 2 * 32)
                    in when (isJust s) $
                       surfaceBlit (fromJust s) Nothing bmpSurface (Just $ P (V2 x y))) $ zip [0..15] surfaces
  filename <- newCString "test.bmp"
  let (Surface ptr _) = bmpSurface
  _ <- saveBMP ptr filename
  return ()
