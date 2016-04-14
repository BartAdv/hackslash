{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}

import Control.Monad
import qualified Data.ByteString as BS
import Data.List (find)
import Data.Maybe
import Data.Vector (Vector, fromList, toList, (!))
import qualified Data.Vector as V
import Debug.Trace

import Foreign.C.String (newCString, withCString)
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
import Level

path :: FilePath
path = "diabdat/levels/towndata/"

testDun = do
  let levelName = "town"
  pal <- BS.readFile (path </> levelName <.> ".pal")
  townCel <- BS.readFile (path </> levelName <.> ".cel")
  let (Right townCels) = loadCel townCel "town"
  Right tils <- BS.readFile (path </> levelName <.> ".til") >>= return . Til.load
  Right mins <- BS.readFile (path </> "town.min") >>= return . Min.load "town"
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

loadCels :: String -> IO (Vector Surface)
loadCels levelName = do
  pal <- BS.readFile (path </> levelName <.> ".pal")
  townCel <- BS.readFile (path </> levelName <.> ".cel")
  let (Right townCels) = loadCel townCel "town"
  celSurfaces townCels pal >>= return . fromList

loadTown = do
  Right pils <- BS.readFile (path </> "town.min") >>= return . Min.load "town"
  Right tils <- BS.readFile (path </> "town.til") >>= return . Til.load
  Right sector1s <- readSector tils pils "sector1s"
  Right sector2s <- readSector tils pils "sector2s"
  Right sector3s <- readSector tils pils "sector3s"
  Right sector4s <- readSector tils pils "sector4s"
  let sectors :: [Dun] = [sector1s, sector2s, sector3s, sector4s]
      w = 16 --sum $ fmap dunColCount sectors
      h = 16 --sum $ fmap dunRowCount sectors
      pillars = [fromSectors sectors (P (V2 x y)) | y <- [0..h-1], x <- [0..w-1]]
  return $ Level w h (fromList pillars)
  where
    fromSectors sectors coords =
      let Just Dun{..} = find (\Dun{..} -> let (P (V2 x y)) = coords - dunStartCoords
                                           in x >= 0 && y >= 0 && x < dunColCount && y < dunRowCount) sectors
          (P (V2 x y)) = coords - dunStartCoords
      in trace (show coords ++ ", " ++ show dunStartCoords) $ dunPillars ! (y * dunColCount + x)
    readSector tils pils name = BS.readFile (path </> name <.> ".dun") >>= return . Dun.load name tils pils
    path = "diabdat/levels/towndata"

isoToScreen :: Num a => Point V2 a -> Point V2 a -> Point V2 a
isoToScreen origin (P (V2 x y)) = origin + (P $ ax * (V2 x x) + ay * (V2 y y))
     where ax = V2 32 16
           ay = V2 (-32) 16

blitPillar :: Vector Surface -> Surface -> Pillar -> (Point V2 Int) -> IO ()
blitPillar celSurfaces targetSurface pillar (fmap fromIntegral -> coords) = do
  mapM_ (\(i, s) -> let (x, y) = (i `mod` 2 * 32, i `div` 2 * 32 - 256)
                        tc = P (V2 x y) + isoToScreen center coords
                    in when (isJust s) $
                       trace (show tc) $ surfaceBlit (fromJust s) Nothing targetSurface (Just tc)) $ V.zip [0..15] pillarFrames
    where
      center = P (V2 320 320)
      pillarFrames :: Vector (Maybe Surface)
      pillarFrames = fmap (maybe Nothing $ \(frameNum, _) -> Just $ celSurfaces ! frameNum) pillar

testLevel :: Vector Surface -> Level -> IO ()
testLevel celSurfaces Level{..} = do
  bmpSurface <- trace "createRGBSurface" $ createRGBSurface (V2 1024 1024) RGBA8888
  mapM_ (\(i, p) -> let coords = P (V2 (i `mod` levelWidth) (i `div`  levelHeight))
                    in when (isJust p) $
                       trace (show coords) $ blitPillar celSurfaces bmpSurface (fromJust p) coords) $ zip [0..] (toList levelPillars)
  withCString "test.bmp" $ \filename -> do
    let (Surface ptr _) = bmpSurface
    _ <- saveBMP ptr filename
    return ()

test = do
  surfaces <- loadCels "town"
  level <- loadTown
  testLevel surfaces level
