module DevelMain (update) where

import Rapid

import Game
import Assets
import Rendering
import SDLEventLoop

update :: IO ()
update = rapid 0 $ \r -> do
  (_window, renderer) <- createRef r "renderer" initializeSDL
  assets <- createRef r "assets" $ loadAssets "diabdat" renderer "foo"
  restart r "loop" $
    sdlHost (Just ticksPerSecond) (game renderer assets)
