#ifndef _FREEABLO_HELPERS
#define _FREEABLO_HELPERS

extern "C" {
  void FAIO_init();

  void Render_init(int32_t, int32_t, int32_t);
  void Render_renderFrame(FARender::SpriteManager* spriteManager, Level::Level* level, LevelObjects* levelObjects, int32_t x, int32_t y)
  void Render_quit();
  Render::LevelObjects* Render_createLevelObjets();
  void Render_destroyLevelObjects(Render::LevelObjects*);
  void Render_moveLevelObject(Render::LevelObjects* levelObjects, int32_t fromX, int32_t fromY, int32_t toX, int32_t toY);
  void Render_setLevelObject(Render::LevelObjects* levelObjects, int32_t x, int32_t y
                             , int32_t valid, uint32_t spriteCacheIndex, size_t spriteFrame, int32_t x2, int32_t y2, int32_t dist);

  FARender::SpriteManager* FARender_createSpriteManager();
  void FARender_destroySpriteManager(FARender::SpriteManager* manager);
  FARender::FASpriteGroup* FARender_loadImage(FARender::SpriteManager* manager, const char*);
  uint32_t FARender_getSpriteCacheIndex(FARender::FASpriteGroup*);
  uint32_t FARender_getSpriteAnimLength(FARender::FASpriteGroup*);
  Level::Level* World_createTownLevel();
  size_t Level_width(Level::Level*);
  size_t Level_height(Level::Level*);
  int32_t Level_passable(Level::Level*, size_t, size_t);
}

#endif
