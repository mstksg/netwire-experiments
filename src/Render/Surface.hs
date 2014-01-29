module Render.Surface where

import Linear.V2
import Render.Sprite
import Linear.Matrix
import Linear.Vector

type Transformation2 = M22 Double

data Entity = EntSurface Surface
            | EntSprite Sprite

data Surface = Surface { surfacePos         :: V2 Double
                       , surfaceTrans       :: Transformation2
                       , surfaceEntities    :: [Entity]
                       }

class HasSurface s where
  toSurface :: s -> Surface

instance HasSurface Sprite where
  toSurface s = Surface zero idTrans [EntSprite s]

toSpriteList :: V2 Double -> Transformation2 -> Surface -> [Sprite]
toSpriteList p t (Surface psur tsur ents) =
    concatMap placeEntity ents
  where
    totP = (t !* psur) ^+^ p
    totT = t !*! tsur
    placeEntity (EntSurface s) = placeSurface s
    placeEntity (EntSprite s) = [placeSprite s]
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

