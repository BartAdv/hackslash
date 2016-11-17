{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Types
  ( Point(..)
  , V2(..)
  , Coord ) where

import Linear.Affine
import Linear.V2

type Coord = Point V2 Int
