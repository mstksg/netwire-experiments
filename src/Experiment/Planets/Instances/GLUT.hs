
module Experiment.Planets.Instances.GLUT where

import Experiment.Planets.Types
import Render.Backend.GLUT
import Render.Surface
import Linear.Vector

instance GLUTRenderable PlanetList where
  renderGLUT = mapM_ renderGLUT . sList
    where
      sList pl = toSpriteList ctr (transScale scale) (toSurface pl)
      sHt      = 2
      ctr      = zero
      scale    = sHt / 20

