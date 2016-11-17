#ifndef _FREEABLO_HELPERS
#define _FREEABLO_HELPERS

extern "C" {
  void FAIO_init();

  FARender::Renderer* Renderer_create(int32_t, int32_t);
  void Renderer_renderFrame(FARender::Renderer*, FAWorld::GameLevel*, int32_t, int32_t);
  void Renderer_destroy();

  Level::Level* World_createTownLevel();
}

#endif
