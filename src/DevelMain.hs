module DevelMain (update) where

import Rapid

import Freeablo
import Game
import SDLEventLoop

update :: IO ()
update = rapid 0 $ \r -> do
  initFAIO
  renderer <- createRef r "renderer" $ createRenderer 1200 1080 False
  town <- createRef r "town" createTownLevel
  restart r "loop" $
    sdlHost (Just ticksPerSecond) (game renderer town)
