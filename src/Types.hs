{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
module Types
  ( Point(..)
  , V2(..)
  , Coord
  , pattern Coord
  , Direction(..)
  , pattern DirS
  , pattern DirSW
  , pattern DirW
  , pattern DirNW
  , pattern DirN
  , pattern DirNE
  , pattern DirE
  , pattern DirSE
  , followDir
  , getDir
  , MoveDist(..)) where

import Linear.Affine
import Linear.V2

type Coord = Point V2 Int
pattern Coord w h = P (V2 w h)

newtype Direction = Direction Int

pattern DirS  = Direction 0
pattern DirSW = Direction 1
pattern DirW  = Direction 2
pattern DirNW = Direction 3
pattern DirN  = Direction 4
pattern DirNE = Direction 5
pattern DirE  = Direction 6
pattern DirSE = Direction 7

instance Show Direction where
  show DirS  = "S"
  show DirSW = "SW"
  show DirW  = "W"
  show DirNW = "NW"
  show DirN  = "N"
  show DirNE = "NE"
  show DirE  = "E"
  show DirSE = "SE"

followDir :: Direction -> Coord -> Coord
followDir dir origin =
  origin + P (V2 ox oy)
  where
    (ox, oy) = case dir of
      DirN  -> (-1, -1)
      DirNE -> (0, -1)
      DirE  -> (1,-1)
      DirSE -> (1, 0)
      DirS  -> (1, 1)
      DirSW -> (0, 1)
      DirW  -> (-1, 1)
      DirNW -> (-1, 0)

getDir :: Coord -> Coord -> Direction
getDir from to =
  case (dx, dy) of
    (-1, -1) -> DirN
    (0, -1)  -> DirNE
    (1,-1)   -> DirE
    (1, 0)   -> DirSE
    (1, 1)   -> DirS
    (0, 1)   -> DirSW
    (-1, 1)  -> DirW
    (-1, 0)  -> DirNW
  where P (V2 dx dy) = from - to

newtype MoveDist = MoveDist Int deriving (Show, Num, Enum, Eq, Ord, Real, Integral)

