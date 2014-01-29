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
-- transShape _ sh = sh

