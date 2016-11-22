{-# LANGUAGE ViewPatterns #-}
module Freeablo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign
import Foreign.C.Types

import Types

newtype Level = Level (Ptr ())
newtype SpriteManager = SpriteManager (Ptr ())
newtype LevelObjects = LevelObjects (Ptr ())

foreign import ccall "freeablo.h FAIO_init" initFAIO :: IO ()

foreign import ccall "freeablo.h Render_init" initRendererFFI :: CInt -> CInt -> CInt -> IO ()
foreign import ccall "freeablo.h Render_quit" quitRendererFFI :: IO ()
foreign import ccall "freeablo.h Render_renderFrame" renderFrameFFI :: Ptr () -> Ptr () -> Ptr () -> CInt -> CInt -> IO ()
foreign import ccall "freeablo.h Render_createLevelObjects" createLevelObjectsFFI :: IO (Ptr ())
foreign import ccall "freeablo.h Render_destroyLevelObjects" destroyLevelObjectsFFI :: Ptr () -> IO ()

foreign import ccall "freeablo.h World_createTownLevel" createTownLevelFFI :: IO (Ptr ())

foreign import ccall "freeablo.h FARender_createSpriteManager" createSpriteManagerFFI :: IO (Ptr ())
foreign import ccall "freeablo.h FARender_destroySpriteManager" destroySpriteManagerFFI :: Ptr () -> IO ()

initRenderer :: MonadIO m => CInt -> CInt -> Bool -> m ()
initRenderer width height fullscreen =
  liftIO $ initRendererFFI width height fullscreen'
  where fullscreen' = if fullscreen then 1 else 0

quitRenderer :: MonadIO m => m ()
quitRenderer = liftIO quitRendererFFI

renderFrame :: MonadIO m => SpriteManager -> Level -> LevelObjects -> Coord -> m ()
renderFrame (SpriteManager sm) (Level l) (LevelObjects objs) (P (fmap fromIntegral -> V2 x y)) =
  liftIO $ renderFrameFFI sm l objs x y

createLevelObjects :: MonadIO m => m LevelObjects
createLevelObjects = liftIO $ LevelObjects <$> createLevelObjectsFFI

destroyLevelObjects :: MonadIO m => LevelObjects -> m ()
destroyLevelObjects (LevelObjects ptr) = liftIO $ destroySpriteManagerFFI ptr

createSpriteManager :: MonadIO m => m SpriteManager
createSpriteManager = liftIO $ SpriteManager <$> createSpriteManagerFFI

destroySpriteManager :: MonadIO m => SpriteManager -> m ()
destroySpriteManager (SpriteManager ptr) = liftIO $ destroySpriteManagerFFI ptr

createTownLevel :: MonadIO m => m Level
createTownLevel = Level <$> liftIO createTownLevelFFI
