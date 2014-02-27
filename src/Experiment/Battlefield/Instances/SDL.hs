{-# OPTIONS -fno-warn-orphans #-}

module Experiment.Battlefield.Instances.SDL where

import Experiment.Battlefield.Types
import Linear.Vector
import Render.Backend.SDL
import Render.Surface
import Data.Map.Strict ((!))
import qualified Graphics.UI.SDL     as SDL
import qualified Graphics.UI.SDL.TTF as SDLTTF

instance SDLRenderable Stage where
  renderSDL scr fm stg = do

      scoreS <-
        SDLTTF.renderTextSolid ka1 textString
          (SDL.Color 255 255 255)

      mapM_ (renderSDL scr fm) sList

      SDL.blitSurface scoreS Nothing scr (Just $ SDL.Rect 20 20 100 50)
      return ()
      
    where
      sList = toSpriteList zero (transScale scale) 1 (toSurface stg)
      ht    = fromIntegral $ SDL.surfaceGetHeight scr
      wd    = fromIntegral $ SDL.surfaceGetWidth scr
      scale = min (ht / h) (wd / w)
      (w,h) = stageDimensions stg
      stgC  = stageScore stg
      (t1sc,t2sc) = stageScoreScores stgC
      ka1 = fm ! FontKA1
      textString = unwords [ "Red" , show t1sc
                           , "Blue", show t2sc
                           , "Time", (show . round) (stageScoreDuration stgC)
                           ]
