{-# LANGUAGE ViewPatterns #-}
module Freeablo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.Types
import Foreign.Ptr

import Types

data Renderer = Renderer (Ptr ())
data Level = Level (Ptr ())

foreign import ccall "freeablo.h FAIO_init" initFAIO :: IO ()

foreign import ccall "freeablo.h Renderer_create" createRendererFFI :: CInt -> CInt -> CInt -> IO (Ptr ())
foreign import ccall "freeablo.h Renderer_renderFrame" renderFrameFFI :: Ptr () -> Ptr () -> CInt -> CInt -> IO ()

foreign import ccall "freeablo.h World_createTownLevel" createTownLevelFFI :: IO (Ptr ())

createRenderer :: MonadIO m => CInt -> CInt -> Bool -> m Renderer
createRenderer width height fullscreen =
  liftIO $ Renderer <$> createRendererFFI width height fullscreen'
  where fullscreen' = if fullscreen then 1 else 0

renderFrame :: MonadIO m => Renderer -> Level -> Coord -> m ()
renderFrame (Renderer r) (Level l) (P (fmap fromIntegral -> V2 x y)) =
  liftIO $ renderFrameFFI r l x y

createTownLevel :: MonadIO m => m Level
createTownLevel = Level <$> liftIO createTownLevelFFI