{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
module Types
  ( Point(..)
  , V2(..)
  , Coord
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
  , MoveDist(..)) where

import Linear.Affine
import Linear.V2

type Coord = Point V2 Int
newtype Direction = Direction Int deriving Show

pattern DirS = Direction 0
pattern DirSW = Direction 1
pattern DirW  = Direction 2
pattern DirNW = Direction 3
pattern DirN  = Direction 4
pattern DirNE = Direction 5
pattern DirE  = Direction 6
pattern DirSE = Direction 7

followDir :: Coord -> Direction -> Coord
followDir origin dir =
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

newtype MoveDist = MoveDist Int deriving Show

