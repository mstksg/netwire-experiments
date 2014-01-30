
module Render.Backend.GLUT where

import Render.Render
import Prelude hiding ((.),id)
import Linear.Vector
-- import Control.Monad
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
    -> Backend (Timed Double ()) e IO (IO ()) a
glutBackend wd ht (cr,cg,cb) = Backend runGLUT
  where
    -- fr = 30
    simDt = 1/30
    sess = countSession_ simDt
    col = Color4
            (fromIntegral cr / 255)
            (fromIntegral cg / 255)
            (fromIntegral cb / 255)
            1

    runGLUT r wr = do
        (progName,_) <- getArgsAndInitialize
        ref <- newIORef (sess,wr)
        initialWindowSize $= Size (fromIntegral wd) (fromIntegral ht)
        initialDisplayMode $= [RGBMode, SingleBuffered]
        createWindow progName
        clearColor $= col
        displayCallback $= display ref
        mainLoop

      where
        display ref = do
          (s',w') <- readIORef ref
          (ds,s) <- stepSession s'
          (mx,w) <- stepWire w' ds (Right NoEvent)
          clear [ColorBuffer]
          case mx of
            Right mx' -> do
              renderGLUT mx'
              r mx'
              writeIORef ref (s,w)
            Left _ -> exit
          flush

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

