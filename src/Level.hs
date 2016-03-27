module Level where

import Data.Vector

data Tile = Tile { tileFoo :: Int }
type Level = Vector Tile
