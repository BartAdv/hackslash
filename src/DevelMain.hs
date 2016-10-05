module DevelMain (update) where

import Rapid

import Game
import Assets
import Rendering
import SDLEventLoop

update :: IO ()
update = rapid 0 $ \r -> do
  (_window, renderer) <- createRef r "renderer" initializeSDL
  assets <- createRef r "assets" $ loadAssets renderer "diabdat/levels/towndata" "foo"
  restart r "loop" $
    sdlHost renderer (Just 60) (game assets)
