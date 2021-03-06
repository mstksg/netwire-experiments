{-# OPTIONS -fno-warn-orphans #-}

module Experiment.Archers.Instances.SDL where

import Render.Backend.SDL
import Experiment.Archers.Types
import Linear.Vector
import Render.Surface
import qualified Graphics.UI.SDL as SDL

instance SDLRenderable Stage where
  renderSDL scr fm stg@(Stage w h _ _) = mapM_ (renderSDL scr fm) sList
    where
      sList = toSpriteList zero (transScale scale) 1 (toSurface stg)
      ht    = fromIntegral $ SDL.surfaceGetHeight scr
      wd    = fromIntegral $ SDL.surfaceGetWidth scr
      scale = min (ht / h) (wd / w)

