{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Level
       (Level(..)
       ,loadTown) where

import Data.List (find)

import Data.Vector (Vector, fromList, (!))
import qualified Data.ByteString as BS
import Linear.Affine (Point(..))
import Linear.V2
import SDL (Renderer)
import System.FilePath.Posix ((</>), (<.>))

import Dat
import Graphics

data Level = Level { levelWidth :: Int
                   , levelHeight :: Int
                   , levelPillars :: Vector (Maybe PillarTexture)
                   , levelPillarHeight :: Int}

loadTown :: Renderer -> FilePath -> IO Level
loadTown renderer path = do
  Right pils <- BS.readFile (path </> "town.min") >>= return . loadMin "town"
  Right tils <- BS.readFile (path </> "town.til") >>= return . loadTil
  Right sector1s <- readSector tils pils "sector1s"
  Right sector2s <- readSector tils pils "sector2s"
  Right sector3s <- readSector tils pils "sector3s"
  Right sector4s <- readSector tils pils "sector4s"
  palette        <- BS.readFile (path </> "town.pal")
  Right cels     <- BS.readFile (path </> "town.cel") >>= return . loadCel "town"
  let sectors :: [Dun] = [sector1s, sector2s, sector3s, sector4s]
      coordMin = minimum $ fmap dunStartCoords sectors
      coordMax = maximum $ fmap (\Dun{..} -> dunStartCoords + (P $ V2 dunColCount dunRowCount)) sectors
      P (V2 w h) = coordMax - coordMin
      pillars = [fromSectors sectors (P (V2 x y)) | y <- [0..h-1], x <- [0..w-1]]
  pillarTextures <- mapM (mapM $ createPillarTexture renderer palette cels) pillars
  let pillarHeight = getBlockCountPerPillar "town" `div` 2
  return $ Level w h (fromList pillarTextures) pillarHeight
  where
    fromSectors sectors coords =
      let Just Dun{..} = find (inSector coords) sectors
          (P (V2 levelX levelY)) = coords - dunStartCoords
      in dunPillars ! (levelY * dunColCount + levelX)
    inSector coords Dun{..} = let (P (V2 x y)) = coords - dunStartCoords
                              in x >= 0 && y >= 0 && x < dunColCount && y < dunRowCount
    readSector tils pils name = BS.readFile (path </> name <.> ".dun") >>= return . loadDun name tils pils
