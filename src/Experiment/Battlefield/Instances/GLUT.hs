{-# OPTIONS -fno-warn-orphans #-}

module Experiment.Battlefield.Instances.GLUT where

import Render.Backend.GLUT
import Experiment.Battlefield.Types
import Linear.V2
import Render.Surface
import Graphics.UI.GLUT

instance GLUTRenderable Stage where
  renderGLUT stg = do
    Size ww wh <- get windowSize

    let
      (w, h)     = stageDimensions stg
      (ww', wh') = (fromIntegral ww, fromIntegral wh)
      ratio = ww' / wh'
      ratio' = w / h
      scl =
        if (ww' / w) > (wh' / h)
          then
            V2 (V2 (2/w/ratio*ratio') 0)
               (V2 0 (2/h))
          else
            V2 (V2 (2/w/ratio) 0)
               (V2 0 (2/h/ratio'))

      sList = toSpriteList (V2 (-1) (-1)) scl 1 (toSurface stg)

    mapM_ renderGLUT sList
