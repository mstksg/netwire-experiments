module Render.Sprite where

import Control.Monad                        (void)
import Data.Word
import Linear.V2
import Linear.Vector
import Render.Backend.SDL
import qualified Graphics.UI.SDL.Primitives as SDL

data Sprite = Sprite SpriteShape (V2 Double) Color

data SpriteShape = Circle Double
type Color = (Word8,Word8,Word8)

class SpriteClass s where
  toSprite :: s -> Sprite

instance SDLRenderable Sprite where
  renderSDL (Sprite sh pos (cr,cg,cb)) origin scl scr =
    case sh of
      Circle r ->
        let r' = r * scl
        in void $ SDL.filledCircle scr (round x') (round y') (round r') col
    where
      V2 x' y' = origin ^+^ pos ^* scl
      col      = rgbColor cr cg cb
    

-- instance SDLRenderable PlanetList where
--   renderSDL (PlanetList ps) origin scl scr = do
--     let
--       ht    = fromIntegral $ SDL.surfaceGetHeight scr
--       wd    = fromIntegral $ SDL.surfaceGetHeight scr
--       ctr   = origin ^+^ V2 ht wd ^/ 2
--       scale = scl * ht / 20
--     mapM_ (\p -> renderSDL p ctr scale scr) ps

-- instance SDLRenderable Planet where
--   renderSDL (Planet _ r (cr,cg,cb) (Body _ (V3 x y _))) origin scale scr =
--     void $ SDL.filledCircle scr (round x') (round y') (round r) col
--     where
--       pos        = V2 x y ^* scale
--       (V2 x' y') = pos ^+^ origin
--       col        = rgbColor cr cg cb

