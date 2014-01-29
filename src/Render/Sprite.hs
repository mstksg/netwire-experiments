module Render.Sprite where

import Linear.V2
import Data.Word
import Linear.Vector
import Linear.Matrix

data Sprite = Sprite { spritePos   :: V2 Double
                     , spriteShape :: SpriteShape
                     , spriteColor :: Color
                     }

data SpriteShape = Circle Double Filling
                 | Rectangle (V2 Double) Filling
                 | Polygon [V2 Double] Filling
                 | Line (V2 Double) (V2 Double)
type Color = (Word8,Word8,Word8)

data Filling = Unfilled | Filled

class HasSprite s where
  toSprite :: s -> Sprite

transSprite :: V2 Double -> M22 Double -> Sprite -> Sprite
transSprite p t (Sprite pspr sh c) = Sprite p' sh' c
  where
    p' = (t !* pspr) ^+^ p
    sh' = transShape t sh

transShape :: M22 Double -> SpriteShape -> SpriteShape
transShape t (Circle r f) = Circle (r * sqrt (det22 t)) f
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
toPolygon (Circle r f)   = toPolygon (Rectangle (V2 r r) f)
toPolygon (Line p1 p2)   = Polygon [p1, p2] Unfilled

fromFilling :: Filling -> a -> a -> a
fromFilling Unfilled f _ = f
fromFilling Filled _ f = f
