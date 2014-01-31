
module Experiment.Planets.Instances.GLUT where

import Experiment.Planets.Types
import Render.Backend.GLUT
import Render.Surface
import Linear.Vector
import Linear.V2
import Graphics.UI.GLUT

instance GLUTRenderable PlanetList where
  renderGLUT pl = do
      Size ww wh <- get windowSize

      let
        ratio = fromIntegral ww / fromIntegral wh
        scale = V2 (V2 (1/ratio) 0)
                   (V2 0 1) ^/ 10

        sList = toSpriteList zero scale (toSurface pl)

      mapM_ renderGLUT sList

