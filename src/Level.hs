module Level
       (Level(..)) where

import Data.Vector

import Dat.Min(Pillar)

data Level = Level { levelWidth :: Int
                   , levelHeight :: Int
                   , levelPillars :: Vector (Maybe Pillar)}
