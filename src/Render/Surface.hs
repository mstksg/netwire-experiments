module Render.Surface where

import Linear.V2
import Render.Sprite
import Linear.Matrix
import Linear.Vector

type Transformation2 = M22 Double

data Surface = Surface { surfacePos         :: V2 Double
                       , surfaceTrans       :: Transformation2
                       , surfaceSprites     :: [Sprite]
                       , surfaceSurfaces    :: [Surface]
                       }

class HasSurface s where
  toSurface :: s -> Surface

toSpriteList :: V2 Double -> Transformation2 -> Surface -> [Sprite]
toSpriteList p t (Surface psur tsur sprs surfs) =
    map placeSprite sprs ++ concatMap placeSurface surfs
  where
    totP = (t !* psur) ^+^ p
    totT = t !*! tsur
    placeSprite = transSprite totP totT
    placeSurface = toSpriteList totP totT

toSpriteList' :: Surface -> [Sprite]
toSpriteList' = toSpriteList zero idTrans

idTrans :: Transformation2
idTrans = V2 (V2 1 0)
             (V2 0 1)

transScale :: Double -> Transformation2
transScale = (*!! idTrans)

transRotate :: Double -> Transformation2
transRotate a = V2 (V2 (cos a) (-1*sin a))
                     (V2 (sin a) (cos a)   )

transScaleRotate :: Double -> Double -> Transformation2
transScaleRotate s a = transRotate a !*! transScale s

