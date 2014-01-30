
module Experiment.Archers.Instances.SDL where


import Render.Backend.SDL
import Experiment.Archers.Types
import Linear.Vector
import Render.Surface
import qualified Graphics.UI.SDL as SDL

instance SDLRenderable Stage where
  renderSDL scr stg@(Stage w h _ _) = mapM_ (renderSDL scr) sList
    where
      sList = toSpriteList zero (transScale scale) (toSurface stg)
      ht    = fromIntegral $ SDL.surfaceGetHeight scr
      wd    = fromIntegral $ SDL.surfaceGetWidth scr
      scale = min (ht / h) (wd / w)

