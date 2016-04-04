{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Vector (Vector, fromList, toList, (!))
import Foreign.C.String (newCString)
import Linear.Affine
import Linear.V2
import SDL hiding (copy)
import SDL.Raw.Video (saveBMP)
import System.FilePath.Posix ((</>), (<.>))

import Dat.Cel
import           Dat.Dun (Dun(..))
import qualified Dat.Dun as Dun
import           Dat.Min (Pillar(..))
import qualified Dat.Min as Min
import qualified Dat.Til as Til

import Graphics

path :: FilePath
path = "diabdat/levels/towndata/"

testDun = do
  let levelName = "town"
  pal <- BS.readFile (path </> levelName <.> ".pal")
  townCel <- BS.readFile (path </> levelName <.> ".cel")
  let (Right townCels) = loadCel townCel "town"
  tils <- BS.readFile (path </> levelName <.> ".til") >>= return . Til.load
  mins <- BS.readFile (path </> "town.min") >>= return . Min.load "town"
  Right Dun{..} <- BS.readFile (path </> "sector1s.dun") >>= return . Dun.load "sector1s" tils mins
  surfaces <- celSurfaces townCels pal >>= return . fromList

  bmpSurface <- createRGBSurface (V2 1024 1024) RGBA8888
  filename <- newCString "test.bmp"
  let (Surface ptr _) = bmpSurface
  _ <- saveBMP ptr filename
  return ()
  where
    pillar :: Vector Min.Pillar -> Maybe Int -> Maybe Min.Pillar
    pillar mins (Just minIdx) = Just $ mins ! minIdx
    pillar _ Nothing = Nothing
    drawPillar :: Vector Surface -> Surface -> Pillar -> (Int, Int) -> IO ()
    drawPillar surfaces target pillar (x, y) =
      mapM_ (\(i, p) -> let (bx, by) = (i `mod` 2 * 32, i `div` 2 * 32)
                            coord = fmap fromIntegral $ P $ V2 (bx + x) (by + y)
                            surface = surfaces ! i
                        in surfaceBlit surface Nothing target (Just coord)) $ zip [0..] (toList pillar)
--

frames :: [Maybe Int]
frames = [Nothing,Nothing,Nothing,Just 1181,Nothing,Just 1182,Nothing,Just 1183,Just 1184,Just 1185,Just 1186,Just 1187,Just 1188,Just 1189,Just 1190,Just 1191]

pillarFrames :: Min.Pillar -> [Maybe Int]
pillarFrames pillar =
  map (fmap (fromIntegral . fst)) $ toList pillar

testCels :: [Maybe Int] -> IO ()
testCels frames = do
  pal <- BS.readFile (path </> "town.pal")
  townCel <- BS.readFile (path </> "town.cel")
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

loadTown = do
  pils <- BS.readFile (path </> "town.min") >>= return . Min.load "town"
  tils <- BS.readFile (path </> "town.til") >>= return . Til.load
  Right sector1s <- readSector tils pils "sector1s"
  Right sector2s <- readSector tils pils "sector2s"
  Right sector3s <- readSector tils pils "sector3s"
  Right sector4s <- readSector tils pils "sector4s"
  return (sector1s, sector2s, sector3s, sector4s)
  where
    readSector tils pils name = BS.readFile (path </> name <.> ".dun") >>= return . Dun.load name tils pils
    path = "diabdat/levels/towndata"
