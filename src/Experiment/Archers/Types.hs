{-# OPTIONS -fno-warn-orphans #-}

module Experiment.Archers.Types where

import Physics
import Linear.V2
import Linear.V3
import Control.Monad.Random
import Render.Sprite
import Render.Surface
import Linear.Vector

data Stage = Stage { stageWidth :: Double
                   , stageHeight :: Double
                   , stageArchers :: [Archer]
                   , stageDarts :: [Dart]
                   }

type Angle = Double

data Archer = Archer  { archerBody :: Body
                      , archerAngle :: Angle
                      }
data Dart = Dart Body Angle

data Message = Die

type Messages = [Message]

isDie :: Message -> Bool
isDie Die = True

instance HasSurface Dart where
  toSurface (Dart (Body _ (V3 x y _)) ang) =
    Surface (V2 x y) (transRotate ang) [EntSprite spr]
    where
      spr = Sprite zero (Line (V2 (-2) 0) (V2 2 0)) (0,0,0)

instance HasSurface Archer where
  toSurface (Archer (Body _ (V3 x y _)) ang) =
    Surface (V2 x y) (transRotate ang) [EntSprite spr]
    where
      spr = Sprite zero (Circle 3 Filled) (0,0,0)

instance HasSurface Stage where
  toSurface (Stage w h arcs arrs) =
      Surface zero idTrans ents
    where
      back  = Sprite (V2 (w/2) (h/2)) (Rectangle (V2 w h) Filled) (1,142,14)
      arrEnts = map (EntSurface . toSurface) arrs
      arcEnts = map (EntSurface . toSurface) arcs
      ents = EntSprite back:(arcEnts ++ arrEnts)

instance (Random x, Random y) => Random (x, y) where
  randomR ((x1, y1), (x2, y2)) gen1 =
    let (x, gen2) = randomR (x1, x2) gen1
        (y, gen3) = randomR (y1, y2) gen2
    in ((x, y), gen3)
  random gen1 =
    let (x, gen2) = random gen1
        (y, gen3) = random gen2
    in ((x, y), gen3)

