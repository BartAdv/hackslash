module DevelMain (update) where

import Rapid

import Freeablo
import Game
import Reflex.SDL.Host

update :: IO ()
update = rapid 0 $ \r -> do
  initFAIO
  _ <- createRef r "renderer" $ initRenderer 1200 1080 False
  spriteManager <- createRef r "spriteManager" createSpriteManager
  town <- createRef r "town" createTownLevel
  restart r "loop" $
    sdlHost (Just ticksPerSecond) (game spriteManager town)
