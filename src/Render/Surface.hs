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
                       , surfaceOpacity     :: Double
                       , surfaceEntities    :: [Entity]
                       }

class HasSurface s where
  toSurface :: s -> Surface

instance HasSurface Sprite where
  toSurface s = Surface zero idTrans 1 [EntSprite s]

emptySurface :: Surface
emptySurface = Surface zero idTrans 1 []

toSpriteList :: V2 Double -> Transformation2 -> Double -> Surface -> [Sprite]
toSpriteList p t o (Surface psur tsur osur ents) =
    concatMap placeEntity ents
  where
    totP = (t !* psur) ^+^ p
    totT = t !*! tsur
    totO = o * osur
    placeEntity (EntSurface s) = placeSurface s
    placeEntity (EntSprite s) = [placeSprite s]
    placeSprite = dimSprite totO . transSprite totP totT
    placeSurface = toSpriteList totP totT totO

toSpriteList' :: Surface -> [Sprite]
toSpriteList' = toSpriteList zero idTrans 1

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

