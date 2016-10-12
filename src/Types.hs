{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Types where

import Linear.Affine (Point)
import Linear.V2

type Coord = Point V2 Int
