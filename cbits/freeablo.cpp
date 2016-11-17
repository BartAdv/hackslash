#include "faio/faio.h"
#include "level/level.h"
#include "faworld/world.h"
#include "farender/renderer.h"

extern "C"
{
  void Renderer_renderFrame(FARender::Renderer* renderer, FAWorld::GameLevel* level, int32_t x, int32_t y)
  {
    FARender::RenderState* state = renderer->getFreeState();
    if(state) {
      state->mPos = FAWorld::Position(x, y);
      if(level != NULL)
        state->tileset = renderer->getTileset(*level);
      state->level = level;
      // if(!FAGui::cursorPath.empty())
      //   state->mCursorEmpty = false;
      // else
      //   state->mCursorEmpty = true;
      // state->mCursorFrame = FAGui::cursorFrame;
      // state->mCursorSpriteGroup = renderer.loadImage("data/inv/objcurs.cel");
            // world.fillRenderState(state);
      // Render::updateGuiBuffer(&state->guiDrawBuffer);
    }
    // else
    //   {
    //     Render::updateGuiBuffer(NULL);
    //   }
    renderer->renderFrame(state);
    // single-threaded rendering:
    state->ready = true;
  }

  FARender::Renderer* Renderer_create(int32_t width, int32_t height, int32_t fullscreen)
  {
    return new FARender::Renderer(width, height, fullscreen != 0);
  }

  void Renderer_destroy(FARender::Renderer* renderer)
  {
    renderer->cleanup(); // ??
    renderer->stop();
    renderer->waitUntilDone();
    delete renderer;
  }

  Level::Level* World_createTownLevel()
  {
    Level::Dun sector1("levels/towndata/sector1s.dun");
    Level::Dun sector2("levels/towndata/sector2s.dun");
    Level::Dun sector3("levels/towndata/sector3s.dun");
    Level::Dun sector4("levels/towndata/sector4s.dun");

    return new Level::Level(Level::Dun::getTown(sector1, sector2, sector3, sector4), "levels/towndata/town.til",
                            "levels/towndata/town.min", "levels/towndata/town.sol", "levels/towndata/town.cel",
                            std::make_pair(25,29), std::make_pair(75,68), std::map<size_t, size_t>(), -1, 1);
  }

  void FAIO_init()
  {
    FAIO::init();
  }
}
