{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module Test where

import Foreign.Store
import Linear.Affine
import Linear.V2
import qualified SDL as SDL

import Assets
import Rendering

path :: FilePath
path = "diabdat/levels/towndata/"

start :: IO (SDL.Window, SDL.Renderer)
start = do
  (window, renderer) <- initializeSDL
  deleteStore $ Store 0
  putStrLn "loading..."
  assets <- loadAssets renderer path "town"
  putStrLn "done."
  assetsStore <- newStore assets
  putStrLn $ "Assets loaded to store" ++ show assetsStore
  putStrLn "done"
  return (window, renderer)

getAssets :: IO Assets
getAssets = do
  Just store <- lookupStore 0 :: IO (Maybe (Store Assets))
  readStore store
