
module Render.Backend.GLUT where

import Render.Render
import Prelude hiding ((.),id)
import Linear.Vector
-- import Control.Monad
import Linear.V2
-- import Data.Time.Clock
import Data.IORef
import Render.Sprite as Sprite
import Render.Surface
import Data.Word
import Control.Wire.Unsafe.Event
import Graphics.UI.GLUT as GLUT
import Control.Wire


glutBackend :: forall e a. GLUTRenderable a
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

        a <- newIORef Nothing
        wireState <- newIORef (sess, wr)

        initialWindowSize $= Size (fromIntegral wd) (fromIntegral ht)
        initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
        createWindow progName
        clearColor $= col
        displayCallback $= display a
        idleCallback $= Just (step wireState a)

        mainLoop

      where
        display :: IORef (Maybe (Either e a)) -> DisplayCallback
        display a = do
          mx <- readIORef a

          case mx of
            Nothing -> return ()
            Just (Right mx') -> do
              clear [ColorBuffer]
              renderGLUT mx'
              r mx'
              swapBuffers
            Just (Left _) -> exit

        step ::
               IORef (Session IO (Timed Double ())
                     , Wire (Timed Double ()) e IO (Event RenderEvent) a)
            -> IORef (Maybe (Either e a))
            -> IdleCallback
        step wireState a = do
          (s',w') <- readIORef wireState
          (ds,s) <- stepSession s'
          (mx,w) <- stepWire w' ds (Right NoEvent)

          writeIORef wireState (s,w)
          writeIORef a (Just mx)

          postRedisplay Nothing

        

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

