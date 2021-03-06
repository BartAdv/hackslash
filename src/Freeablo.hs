{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Freeablo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign
import Foreign.C.Types
import Foreign.C.String

import Types

newtype Level = Level (Ptr ())
newtype SpriteManager = SpriteManager (Ptr ())
newtype LevelObjects = LevelObjects (Ptr ())
newtype SpriteGroup = SpriteGroup (Ptr ())

foreign import ccall "freeablo.h FAIO_init" initFAIO :: IO ()

foreign import ccall "freeablo.h Render_init" initRendererFFI :: CInt -> CInt -> CInt -> IO ()
foreign import ccall "freeablo.h Render_quit" quitRendererFFI :: IO ()
foreign import ccall "freeablo.h Render_renderFrame" renderFrameFFI :: Ptr () -> Ptr () -> Ptr () -> CInt -> CInt -> IO ()
foreign import ccall "freeablo.h Render_createLevelObjects" createLevelObjectsFFI :: IO (Ptr ())
foreign import ccall "freeablo.h Render_destroyLevelObjects" destroyLevelObjectsFFI :: Ptr () -> IO ()
foreign import ccall "freeablo.h Render_moveLevelObject" moveLevelObjectFFI :: Ptr () -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "freeablo.h Render_setLevelObject" setLevelObjectFFI :: Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "freeablo.h World_createTownLevel" createTownLevelFFI :: IO (Ptr ())

foreign import ccall "freeablo.h Level_width" getWidthFFI :: Ptr () -> IO CInt
foreign import ccall "freeablo.h Level_height" getHeightFFI :: Ptr () -> IO CInt

foreign import ccall "freeablo.h Level_passable" isPassableFFI :: Ptr () -> CInt -> CInt -> IO CInt

foreign import ccall "freeablo.h FARender_createSpriteManager" createSpriteManagerFFI :: IO (Ptr ())
foreign import ccall "freeablo.h FARender_destroySpriteManager" destroySpriteManagerFFI :: Ptr () -> IO ()
foreign import ccall "freeablo.h FARender_loadImage" loadImageFFI :: Ptr () -> CString -> IO (Ptr ())
foreign import ccall "freeablo.h FARender_getSpriteCacheIndex" getSpriteCacheIndexFFI :: Ptr () -> IO CInt
foreign import ccall "freeablo.h FARender_getSpriteAnimLength" getSpriteAnimLengthFFI :: Ptr () -> IO CInt

initRenderer :: MonadIO m => CInt -> CInt -> Bool -> m ()
initRenderer width height fullscreen =
  liftIO $ initRendererFFI width height fullscreen'
  where fullscreen' = if fullscreen then 1 else 0

quitRenderer :: MonadIO m => m ()
quitRenderer = liftIO quitRendererFFI

pattern CoordXY x y <- P (fmap fromIntegral -> V2 x y)

renderFrame :: MonadIO m => SpriteManager -> Level -> LevelObjects -> Coord -> m ()
renderFrame (SpriteManager sm) (Level l) (LevelObjects objs) (CoordXY x y) =
  liftIO $ renderFrameFFI sm l objs x y

createLevelObjects :: MonadIO m => m LevelObjects
createLevelObjects = liftIO $ LevelObjects <$> createLevelObjectsFFI

destroyLevelObjects :: MonadIO m => LevelObjects -> m ()
destroyLevelObjects (LevelObjects ptr) = liftIO $ destroySpriteManagerFFI ptr

moveLevelObject :: MonadIO m => LevelObjects -> Coord -> Coord -> m Coord
moveLevelObject (LevelObjects objs) from@(CoordXY fromX fromY) to@(CoordXY toX toY) = do
  res <- liftIO $ moveLevelObjectFFI objs fromX fromY toX toY
  pure $ if res /= 0 then to else from

newtype SpriteCacheIndex = SpriteCacheIndex CInt

updateLevelObject :: MonadIO m => LevelObjects -> Coord -> SpriteCacheIndex -> Int -> Coord -> MoveDist -> m ()
updateLevelObject (LevelObjects pObjs) (CoordXY x y) (SpriteCacheIndex (fromIntegral -> spriteCacheIndex)) (fromIntegral -> spriteFrame) (CoordXY x2 y2) (MoveDist dist) =
  liftIO $ setLevelObjectFFI pObjs x y 1 spriteCacheIndex spriteFrame x2 y2 (fromIntegral dist)

createSpriteManager :: MonadIO m => m SpriteManager
createSpriteManager = liftIO $ SpriteManager <$> createSpriteManagerFFI

destroySpriteManager :: MonadIO m => SpriteManager -> m ()
destroySpriteManager (SpriteManager pMgr) = liftIO $ destroySpriteManagerFFI pMgr

loadImage :: MonadIO m => SpriteManager -> String -> m SpriteGroup
loadImage (SpriteManager pMgr) path = liftIO $ SpriteGroup <$> withCString path (loadImageFFI pMgr)

getSpriteCacheIndex :: MonadIO m => SpriteGroup -> m SpriteCacheIndex
getSpriteCacheIndex (SpriteGroup ptr) = liftIO $ SpriteCacheIndex <$> getSpriteCacheIndexFFI ptr

getSpriteAnimLength :: MonadIO m => SpriteGroup -> m Int
getSpriteAnimLength (SpriteGroup ptr) = liftIO $ fromIntegral <$> getSpriteAnimLengthFFI ptr

createTownLevel :: MonadIO m => m Level
createTownLevel = Level <$> liftIO createTownLevelFFI

isPassable :: MonadIO m => Level -> Coord -> m Bool
isPassable (Level ptr) (CoordXY x y) = liftIO $ do
  res <- isPassableFFI ptr x y
  pure $ res /= 0

levelSize :: MonadIO m => Level -> m (V2 Int)
levelSize (Level ptr) = liftIO $ do
  res <- V2 <$> getWidthFFI ptr <*> getHeightFFI ptr
  pure $ fmap fromIntegral res
