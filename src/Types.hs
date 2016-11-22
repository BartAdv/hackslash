{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Types
  ( Point(..)
  , V2(..)
  , Coord
  , Direction ) where

import Linear.Affine
import Linear.V2

type Coord = Point V2 Int
newtype Direction = Direction Int
