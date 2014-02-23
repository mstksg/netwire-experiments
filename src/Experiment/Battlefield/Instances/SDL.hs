{-# OPTIONS -fno-warn-orphans #-}

module Experiment.Battlefield.Instances.SDL where

import Experiment.Battlefield.Types
import Render.Backend.SDL
import Render.Surface
import Linear.Vector
import qualified Graphics.UI.SDL    as SDL

instance SDLRenderable Stage where
  renderSDL scr stg = mapM_ (renderSDL scr) sList
    where
      sList = toSpriteList zero (transScale scale) 1 (toSurface stg)
      ht    = fromIntegral $ SDL.surfaceGetHeight scr
      wd    = fromIntegral $ SDL.surfaceGetWidth scr
      scale = min (ht / h) (wd / w)
      (w,h) = stageDimensions stg
