#ifndef _FREEABLO_HELPERS
#define _FREEABLO_HELPERS

extern "C" {
  void FAIO_init();

  void Render_init(int32_t, int32_t, int32_t);
  void Render_renderFrame(FARender::SpriteManager* spriteManager, Level::Level* level, LevelObjects* levelObjects, int32_t x, int32_t y)
  void Render_quit();
  Render::LevelObjects* Render_createLevelObjets();
  void Render_destroyLevelObjects(Render::LevelObjects*);

  FARender::SpriteManager* FARender_createSpriteManager();
  void FARender_destroySpriteManager(FARender::SpriteManager* manager);

  Level::Level* World_createTownLevel();
}

#endif
