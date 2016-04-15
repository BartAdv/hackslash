{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module Test where

import Control.Monad
import qualified Data.ByteString as BS
import Data.List (find)
import Data.Maybe
import Data.Vector (Vector, fromList, toList, (!))
import qualified Data.Vector as V


import Foreign.C.String (newCString, withCString)
import Foreign.Store
import Linear.Affine
import Linear.V2
import SDL
import SDL.Raw.Video (saveBMP)
import System.FilePath.Posix ((</>), (<.>))

import Dat.Cel
import           Dat.Dun (Dun(..))
import qualified Dat.Dun as Dun
import           Dat.Min (Pillar(..))
import qualified Dat.Min as Min
import           Dat.Til
import qualified Dat.Pal as Pal
import Graphics
import Level

path :: FilePath
path = "diabdat/levels/towndata/"

frames :: [Maybe Int]
frames = [Nothing,Nothing,Nothing,Just 1181,Nothing,Just 1182,Nothing,Just 1183,Just 1184,Just 1185,Just 1186,Just 1187,Just 1188,Just 1189,Just 1190,Just 1191]

pillarFrames :: Min.Pillar -> [Maybe Int]
pillarFrames pillar =
  map (fmap (fromIntegral . fst)) $ toList pillar

loadCels :: String -> IO (Vector DecodedCel)
loadCels levelName = do
  pal <- BS.readFile (path </> levelName <.> ".pal")
  townCel <- BS.readFile (path </> levelName <.> ".cel")
  let (Right townCels) = loadCel townCel "town"
  return $ fromList townCels

readSector tils pils name = BS.readFile (path </> name <.> ".dun") >>= return . Dun.load name tils pils

-- printDun = do
--   Right pils <- BS.readFile (path </> "town.min") >>= return . Min.load "town"
--   Right tils <- BS.readFile (path </> "town.til") >>= return . loadTil
--   Right sector1s <- readSector tils pils "sector1s"

loadTown = do
  Right pils <- BS.readFile (path </> "town.min") >>= return . Min.load "town"
  Right tils <- BS.readFile (path </> "town.til") >>= return . loadTil
  Right sector1s <- readSector tils pils "sector1s"
  Right sector2s <- readSector tils pils "sector2s"
  Right sector3s <- readSector tils pils "sector3s"
  Right sector4s <- readSector tils pils "sector4s"
  let sectors :: [Dun] = [sector1s, sector2s, sector3s, sector4s]
      coordMin = minimum $ fmap dunStartCoords sectors
      coordMax = maximum $ fmap (\Dun{..} -> dunStartCoords + (P $ V2 dunColCount dunRowCount)) sectors
      P (V2 w h) = coordMax - coordMin
      pillars = [fromSectors sectors (P (V2 x y)) | y <- [0..h-1], x <- [0..w-1]]
  return $ Level w h (fromList pillars)
  where
    fromSectors sectors coords =
      let Just Dun{..} = find (\Dun{..} -> let (P (V2 x y)) = coords - dunStartCoords
                                           in x >= 0 && y >= 0 && x < dunColCount && y < dunRowCount) sectors
          (P (V2 x y)) = coords - dunStartCoords
      in dunPillars ! (y * dunColCount + x)

isoToScreen :: Num a => Point V2 a -> Point V2 a -> Point V2 a
isoToScreen origin (P (V2 x y)) = origin + (P $ ax * (V2 x x) + ay * (V2 y y))
     where ax = V2 32 16
           ay = V2 (-32) 16

blitPillar :: Vector Surface -> Surface -> Pillar -> (Point V2 Int) -> IO ()
blitPillar celSurfaces targetSurface pillar (fmap fromIntegral -> coords) = do
  mapM_ (\(i, s) -> let (x, y) = (i `mod` 2 * 32, i `div` 2 * 32 - 256)
                        tc = P (V2 x y) + isoToScreen center coords
                    in when (isJust s) $
                       surfaceBlit (fromJust s) Nothing targetSurface (Just tc)) $ V.zip [0..15] pillarFrames
    where
      center = P (V2 3000 32)
      pillarFrames :: Vector (Maybe Surface)
      pillarFrames = fmap (maybe Nothing $ \(frameNum, _) -> Just $ celSurfaces ! frameNum) pillar

testLevel :: Vector Surface -> Level -> IO ()
testLevel celSurfaces Level{..} = do
  bmpSurface <- createRGBSurface (V2 6000 3100) RGBA8888
  mapM_ (\(i, p) -> let coords = P (V2 (i `mod` levelWidth) (i `div`  levelHeight))
                    in when (isJust p) $
                       blitPillar celSurfaces bmpSurface (fromJust p) coords) $ zip [0..] (toList levelPillars)
  withCString "test.bmp" $ \filename -> do
    let (Surface ptr _) = bmpSurface
    _ <- saveBMP ptr filename
    return ()

data Assets = Assets
  { assetsCels :: Vector DecodedCel
  , assetsPalette :: Pal.Palette
  , assetsLevel :: Level }

loadAssets levelName = do
  cels <- loadCels levelName
  pal <- BS.readFile (path </> levelName <.> ".pal")
  level <- loadTown
  newStore $ Assets cels pal level

getAssets = do
  Just store <- lookupStore 0 :: IO (Maybe (Store Assets))
  readStore store

testSDL :: IO ()
testSDL = do
  SDL.initializeAll

  let winConfig = SDL.defaultWindow { SDL.windowPosition = SDL.Absolute (P (V2 100 100))
                                    , SDL.windowInitialSize = V2 640 480 }

      rdrConfig = SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedVSyncRenderer
                                     , SDL.rendererTargetTexture = True }

  window <- SDL.createWindow "Testing" winConfig
  renderer <- SDL.createRenderer window (-1) rdrConfig

  Assets{..} <- getAssets

  let pillar = fromJust $ (levelPillars assetsLevel) ! 0
  PillarTexture{..} <- createPillarTexture renderer assetsPalette assetsCels pillar

  SDL.clear renderer
  SDL.copy renderer pillarTexture Nothing (Just $ SDL.Rectangle (P $ V2 32 32) pillarTextureSize)
  SDL.present renderer

  SDL.delay 4000

  SDL.destroyTexture pillarTexture
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  SDL.quit
