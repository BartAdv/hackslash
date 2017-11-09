module DevelMain (update) where

import Rapid

import Freeablo
import Game
import Reflex.SDL2

update :: IO ()
update = rapid 0 $ \r -> do
  initFAIO
  _ <- createRef r "renderer" $ initRenderer 1920 1080 False
  spriteManager <- createRef r "spriteManager" createSpriteManager
  town <- createRef r "town" createTownLevel
  restart r "loop" $
    host () (game spriteManager town)
