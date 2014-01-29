module Render.Sprite where

import Linear.V2
import Data.Word
import Linear.Vector
import Linear.Matrix

data Sprite = Sprite { spritePos   :: V2 Double
                     , spriteShape :: SpriteShape
                     , spriteColor :: Color
                     }

data SpriteShape = Circle Double
                 | Rectangle (V2 Double)
                 | Polygon [V2 Double]
type Color = (Word8,Word8,Word8)

class HasSprite s where
  toSprite :: s -> Sprite

transSprite :: V2 Double -> M22 Double -> Sprite -> Sprite
transSprite p t (Sprite pspr sh c) = Sprite p' sh' c
  where
    p' = (t !* pspr) ^+^ p
    sh' = transShape t sh

transShape :: M22 Double -> SpriteShape -> SpriteShape
transShape t (Circle r) = Circle (r * sqrt (det22 t))
transShape t r@(Rectangle (V2 w h)) =
  case maybeDiagonal t of
    Just (V2 x y) -> Rectangle (V2 (w*x) (h*y))
    Nothing       -> transShape t (toPolygon r)
transShape t (Polygon vs) = Polygon (map (t !*) vs)

maybeDiagonal :: M22 Double -> Maybe (V2 Double)
maybeDiagonal (V2 (V2 x 0) (V2 0 y)) = Just (V2 x y)
maybeDiagonal _ = Nothing

toPolygon :: SpriteShape -> SpriteShape
toPolygon (Rectangle (V2 w h)) = Polygon [ V2 (-w/2) (-h/2)
                                         , V2 ( w/2) (-h/2)
                                         , V2 ( w/2) ( h/2)
                                         , V2 (-w/2) ( h/2) ]
toPolygon (Polygon vs) = Polygon vs
toPolygon (Circle r)   = toPolygon (Rectangle (V2 r r))
