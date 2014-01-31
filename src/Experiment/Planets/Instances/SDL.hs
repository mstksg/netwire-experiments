{-# OPTIONS -fno-warn-orphans #-}

module Experiment.Planets.Instances.SDL where

import Render.Backend.SDL
import Experiment.Planets.Types
import Linear.Vector
import Render.Surface
import Linear.V2
import qualified Graphics.UI.SDL as SDL

instance SDLRenderable PlanetList where
  renderSDL scr = mapM_ (renderSDL scr) . sList
    where
      sList pl = toSpriteList ctr (transScale scale) (toSurface pl)
      ht       = fromIntegral $ SDL.surfaceGetHeight scr
      wd       = fromIntegral $ SDL.surfaceGetHeight scr
      ctr      = V2 ht wd ^/ 2
      scale    = ht / 20

