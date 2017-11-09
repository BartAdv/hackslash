{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
module Path where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Graph.AStar
import Data.Hashable
import Data.HashSet (HashSet, fromList)
import Data.Ix
import qualified Data.List as List
import GHC.Generics
import Linear.Metric (distance)
import System.IO.Unsafe

import Debug.Trace

import Freeablo
import Types

type Path = [Coord]

findPath :: Level -> Coord -> Coord -> Maybe Path
findPath level start target =
  aStar graph dist (heuristic target) (== target) (trace ("findPath: " ++ show start ++ " -> " ++ show target) start)
  where
    graph :: Coord -> HashSet Coord
    graph coord =
      let bounds = (Coord 0 0, ) . P <$> unsafePerformIO $ levelSize level
          vertices = filter (unsafePerformIO . isPassable level) (neighbours bounds)
      in fromList vertices
      where
        neighbours bounds =
          let coords = filter (/= coord) $ range (coord - 1, coord + 1)
          in filter (inRange bounds) coords

    dist :: Coord -> Coord -> Double
    dist _ _ = 1

    -- untested
    heuristic :: Coord -> Coord -> Double
    heuristic a b = distance (fromIntegral <$> a) (fromIntegral <$> b)
