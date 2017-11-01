{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
module Path where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Graph.AStar
import Data.Hashable
import Data.HashSet (HashSet, fromList)
import Data.Ix
import GHC.Generics
import System.IO.Unsafe

import Freeablo
import Types

type Path = [Coord]

{-# NOINLINE findPath #-}
findPath :: Level -> Coord -> Coord -> Maybe Path
findPath level start target =
  -- whooops :S
  unsafePerformIO $ aStarM graph dist (heuristic target) (pure . (== target)) (pure start)
  where
    graph :: MonadIO m => Coord -> m (HashSet Coord)
    graph coord = do
      bounds <- liftIO $ (Coord 0 0, ) . P <$> levelSize level
      cells <- mapM (\c -> isPassable level c >>= pure . (c,)) (neighbours bounds)
      let vertices = filter snd cells
      pure $ fromList $ fst <$> vertices
      where
        neighbours bounds =
          let coords = filter (/= coord) $ range (coord - 1, coord + 1)
          in filter (inRange bounds) coords

    dist :: MonadIO m => Coord -> Coord -> m Int
    dist _ _ = pure 1

    -- untested
    heuristic :: MonadIO m => Coord -> Coord -> m Int
    heuristic target@(P (V2 tx ty)) (P (V2 x y)) = do
      let dx = abs x - tx
          dy = abs y - ty
      pure $ (dx + dy) - min dx dy
