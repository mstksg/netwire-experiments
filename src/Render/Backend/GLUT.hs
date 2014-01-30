
module Render.Backend.GLUT where

import Render.Render
import Prelude hiding ((.),id)
import Linear.Vector
import Control.Monad
import Linear.V2
import Data.IORef
import Render.Sprite as Sprite
import Render.Surface
import Data.Word
import Control.Wire.Unsafe.Event
import Graphics.UI.GLUT as GLUT
import Control.Wire


glutBackend :: GLUTRenderable a
    => Int
    -> Int
    -> (Word8, Word8, Word8)
    -> Backend (Timed Double ()) e IO (()) a
glutBackend wd ht (cr,cg,cb) = Backend runGLUT
  where
    fr = 30
    simDt = 1/30
    sess = countSession_ simDt
    runGLUT r wr = do
      (progName,_) <- getArgsAndInitialize
      ref <- newIORef (sess,wr)
      initialWindowSize $= Size (fromIntegral wd) (fromIntegral ht)
      initialDisplayMode $= [RGBMode, SingleBuffered]
      createWindow progName
      displayCallback $= display ref
      return ()

    display ref = do
      (s',w') <- readIORef ref
      (ds,s) <- stepSession s'
      (mx,w) <- stepWire w' ds (Right NoEvent)
      case mx of
        Right mx' -> do
          clear [ColorBuffer]
          renderGLUT mx'
          flush
          writeIORef ref (s,w)
        Left _ -> exit

class GLUTRenderable s where
  renderGLUT :: s -> IO ()

instance GLUTRenderable Surface where
  renderGLUT = mapM_ renderGLUT  . toSpriteList'


instance GLUTRenderable Sprite where
  renderGLUT s@(Sprite o sh (cr,cg,cb)) =
    case sh of
      Sprite.Polygon vs f ->
        let shapeType =
              case f of Filled -> GLUT.Polygon
                        Unfilled -> GLUT.LineLoop
        in renderPrimitive shapeType $ do
             color col
             mapM_ (vertex . v2ToVertex2 . (^+^ o)) vs
      Sprite.Line v0 v1 ->
        renderPrimitive GLUT.Lines $ do
          color col
          vertex (v2ToVertex2 (o ^+^ v0))
          vertex (v2ToVertex2 (o ^+^ v1))
      _ -> renderGLUT (s {spriteShape = toPolygon sh})
    where
      col :: Color3 GLfloat
      col = Color3
              (fromIntegral cr / 255)
              (fromIntegral cg / 255)
              (fromIntegral cb / 255)

v2ToVertex2 :: Real a => V2 a -> Vertex2 GLfloat
v2ToVertex2 (V2 x y) = Vertex2 (realToFrac x) (realToFrac y)

        
    -- where
    --   drawer = case sh of
    --     Circle r f ->
    --       let drawFunc = fromFilling f SDL.circle SDL.filledCircle
    --       in  drawFunc scr (round x) (round y) (round r)
    --     Rectangle (V2 w h) f ->
    --       let drawFunc = fromFilling f SDL.rectangle SDL.box
    --       in  drawFunc scr
    --             (SDL.Rect
    --               (round (x-(w/2)))
    --               (round (y-(h/2)))
    --               (round (x+(w/2)))
    --               (round (y+(h/2))))
    --     Polygon vs f -> drawFunc scr (map toTup vs)
    --       where
    --         toTup (V2 x' y') = (round (x+x'), round (y+y'))
    --         drawFunc = fromFilling f SDL.polygon SDL.filledPolygon
    --     Line (V2 x0 y0) (V2 x1 y1)
    --       | x0 == x1  ->
    --           SDL.vLine scr (round (x+x0)) (round (y+y0)) (round (y+y1))
    --       | y0 == y1  ->
    --           SDL.hLine scr (round (x+x0)) (round (x+x1)) (round (y+y0))
    --       | otherwise ->
    --           SDL.line scr
    --             (round (x+x0)) (round (y+y0))
    --             (round (x+x1)) (round (y+y1))
    --   col = rgbColor cr cg cb
