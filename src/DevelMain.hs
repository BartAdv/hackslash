module DevelMain (update) where

import Rapid

import Game
import Assets
import Rendering
import ReflexSDL

update :: IO ()
update = rapid 0 $ \r -> do
  (_window, renderer) <- createRef r "renderer" initializeSDL
  assets <- createRef r "assets" $ loadAssets renderer "diabdat/levels/towndata" "foo"
  restart r "loop" $
    host game (renderGame renderer assets)
