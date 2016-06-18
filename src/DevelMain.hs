module DevelMain (update) where

import FRP.Yampa
import Rapid
import System.Random (newStdGen)

import Game
import Input
import Assets
import Rendering

update :: IO ()
update = rapid 0 $ \r -> do
  g <- newStdGen
  (_window, renderer) <- createRef r "resources" initializeSDL
  assets <- createRef r "assets" $ loadAssets renderer "diabdat/levels/towndata" "foo"
  restart r "loop" $
    animate assets renderer $ parseWinInput >>> (game g &&& handleExit)
