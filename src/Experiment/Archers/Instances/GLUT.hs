
module Experiment.Archers.Instances.GLUT where

import Render.Backend.GLUT
import Experiment.Archers.Types
import Linear.Vector
import Linear.V2
import Render.Surface
import Graphics.UI.GLUT

instance GLUTRenderable Stage where
  renderGLUT stg@(Stage w h _ _) = do
    Size ww wh <- get windowSize

    let
      ratio = fromIntegral ww / fromIntegral wh
      scale = V2 (V2 (2/ratio/w) 0)
                  (V2 0 (2/h))

      sList = toSpriteList (V2 (-1) (-1)) scale (toSurface stg)

    mapM_ renderGLUT sList
