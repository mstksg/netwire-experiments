module Render.Sprite where

import Linear.V2
-- import Data.Word
import Control.Applicative (pure, (<*>), (<$>))
import Linear.Vector
import Linear.Matrix
import Data.Colour

data Sprite = Sprite { spritePos     :: V2 Double
                     , spriteShape   :: SpriteShape
                     , spriteColor   :: Color
                     , spriteOpacity :: Double
                     }

data SpriteShape = Circle Double Filling
                 | Ellipse (V2 Double) Filling
                 | Rectangle (V2 Double) Filling
                 | Polygon [V2 Double] Filling
                 | Line (V2 Double) (V2 Double)

type Color = Colour Double

data Filling = Unfilled | Filled

class HasSprite s where
  toSprite :: s -> Sprite

transSprite :: V2 Double -> M22 Double -> Sprite -> Sprite
transSprite p t (Sprite pspr sh c o) = Sprite p' sh' c o
  where
    p' = (t !* pspr) ^+^ p
    sh' = transShape t sh

dimSprite :: Double -> Sprite -> Sprite
dimSprite x s = s { spriteOpacity = o * x }
  where
    o = spriteOpacity s

transShape :: M22 Double -> SpriteShape -> SpriteShape
transShape t c@(Circle r f) =
  case maybeDiagonal t of
    Just t'@(V2 x y) | x == y    -> Circle (r * x) f
                     | otherwise -> Ellipse (r *^ t') f
    Nothing                      -> transShape t (toPolygon c)
transShape t e@(Ellipse r f) =
    case maybeDiagonal t of
      Just r' -> Ellipse ((*) <$> r' <*> r) f
      Nothing -> transShape t (toPolygon e)
transShape t r@(Rectangle (V2 w h) f) =
  case maybeDiagonal t of
    Just (V2 x y) -> Rectangle (V2 (w*x) (h*y)) f
    Nothing       -> transShape t (toPolygon r)
transShape t (Polygon vs f) = Polygon (map (t !*) vs) f
transShape t (Line p1 p2) = Line (t !* p1) (t !* p2)

maybeDiagonal :: M22 Double -> Maybe (V2 Double)
maybeDiagonal (V2 (V2 x 0) (V2 0 y)) = Just (V2 x y)
maybeDiagonal _ = Nothing

toPolygon :: SpriteShape -> SpriteShape
toPolygon (Rectangle (V2 w h) f) = Polygon [ V2 (-w/2) (-h/2)
                                           , V2 ( w/2) (-h/2)
                                           , V2 ( w/2) ( h/2)
                                           , V2 (-w/2) ( h/2) ] f
toPolygon (Polygon vs f) = Polygon vs f
toPolygon (Line p1 p2)   = Polygon [p1, p2] Unfilled
toPolygon (Circle r f)   = toPolygon (Ellipse (pure r) f)
toPolygon (Ellipse (V2 rx ry) f) = Polygon (map angleToEllipse [0..360]) f
  where
    angleToEllipse d =
      let a = d / 180 * pi
      in  V2 (rx * cos a) (ry * sin a)

fromFilling :: Filling -> a -> a -> a
fromFilling Unfilled f _ = f
fromFilling Filled _ f = f
