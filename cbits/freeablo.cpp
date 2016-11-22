#include "faio/faio.h"
#include "level/level.h"
#include "render/levelobjects.h"
#include "render/render.h"
#include "faworld/world.h"
#include "farender/renderer.h"

extern "C"
{
  void Render_renderFrame(FARender::SpriteManager* spriteManager, Level::Level* level, Render::LevelObjects* levelObjects, int32_t x, int32_t y)
  {
    auto minTops = spriteManager->getTileset(level->getTileSetPath(), level->getMinPath(), true);
    auto minBottoms = spriteManager->getTileset(level->getTileSetPath(), level->getMinPath(), false);

    if(levelObjects->width() != level->width() || levelObjects->height() != level->height())
      levelObjects->resize(level->width(), level->height());

    Render::drawLevel(*level, minTops->getCacheIndex(), minBottoms->getCacheIndex(), spriteManager, *levelObjects, x, y
                      // no smooth scrolling
                      , x, y, 0);

    Render::draw();
  }

  void Render_init(int32_t width, int32_t height, int32_t fullscreen)
  {

    Render::RenderSettings settings;
    settings.windowWidth = width;
    settings.windowHeight = height;
    settings.fullscreen = fullscreen;

    Render::init(settings);
  }

  void Render_quit()
  {
    Render::quit();
  }

  Render::LevelObjects* Render_createLevelObjects()
  {
    return new Render::LevelObjects();
  }

  void Render_destroyLevelObjects(Render::LevelObjects* levelObjects)
  {
    delete levelObjects;
  }

  FARender::SpriteManager* FARender_createSpriteManager()
  {
    return new FARender::SpriteManager(1024);
  }

  void FARender_destroySpriteManager(FARender::SpriteManager* manager)
  {
    delete manager;
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
