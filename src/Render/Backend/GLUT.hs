
module Render.Backend.GLUT where

-- import Control.Monad
-- import Data.Time.Clock
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.IORef
import Data.Word
import Data.Char
import Graphics.UI.GLUT          as GLUT
import Linear.V2
import Linear.Vector
import Prelude hiding            ((.),id)
import Render.Render
import Render.Sprite             as Sprite
import Render.Surface
import qualified Data.Sequence   as S

-- data GLUTState = GLUTState { glutState }


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

        a         <- newIORef Nothing
        wireState <- newIORef (sess, wr)
        evs       <- newIORef mempty
        status    <- newIORef mempty

        initialWindowSize $= Size (fromIntegral wd) (fromIntegral ht)
        initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
        createWindow progName
        clearColor $= col
        displayCallback $= display status a
        idleCallback $= Just (step status evs wireState a)
        keyboardMouseCallback $= Just (keyboardMouse evs)

        mainLoop

      where
        display :: IORef String -> IORef (Maybe (Either e a)) -> DisplayCallback
        display status a = do
          mx <- readIORef a

          case mx of
            Nothing -> return ()
            Just (Right mx') -> do
              clear [ColorBuffer]
              renderGLUT mx'

              stat <- readIORef status
              currentRasterPosition $= Vertex4 (-0.95) (0.95) 0 1
              renderString Fixed8By13 stat

              r mx'
              swapBuffers
            Just (Left _) -> exit

        step ::
               IORef String
            -> IORef (S.Seq RenderEvent)
            -> IORef (Session IO (Timed Double ())
                     , Wire (Timed Double ()) e IO (Event RenderEvent) a)
            -> IORef (Maybe (Either e a))
            -> IdleCallback
        step status evs wireState a = do
          (s',w') <- readIORef wireState
          (ds,s) <- stepSession s'

          evseq <- readIORef evs
          ev <- case S.viewl evseq of
                  S.EmptyL        ->
                    return NoEvent
                  ev' S.:< evseq' -> do
                    writeIORef evs evseq'
                    return (Event ev')


          -- case ev of
          --   NoEvent -> return ()
          --   Event e -> writeIORef status (show e)

          (mx,w) <- stepWire w' ds (Right ev)

          writeIORef wireState (s,w)
          writeIORef a (Just mx)

          postRedisplay Nothing


        keyboardMouse :: IORef (S.Seq RenderEvent) -> KeyboardMouseCallback
        keyboardMouse evs key kstate mods (Position x y) = do
            case ev of
              RenderNullEvent -> return ()
              ev'             -> modifyIORef' evs (S.|> ev')
          where
            upDown (d,u) = case kstate of
                             Down -> d
                             Up -> u
            pos = (fromIntegral x, fromIntegral y)
            ev =
              case key of
                MouseButton b ->
                  let b' = case b of
                             LeftButton -> RenderMouseLeft
                             MiddleButton -> RenderMouseMiddle
                             RightButton -> RenderMouseRight
                             WheelUp -> RenderMouseWheelUp   -- doesn't work for some reason
                             WheelDown -> RenderMouseWheelDown
                             _ -> RenderMouseButtonUnknown
                  in  (upDown (RenderMouseDown,RenderMouseUp)) pos b'
                SpecialKey _ -> RenderUnknownEvent
                GLUT.Char c ->
                  let mods' = zip
                                (map ($ mods) [shift,alt,ctrl])
                                [RenderKeyShift ..]
                      modList = map snd . filter ((== Down) . fst) $ mods'
                      kData = RenderKeyData (ord c) modList
                  in  (upDown (RenderKeyDown,RenderKeyUp)) kData


-- data RenderEvent = RenderKeyDown RenderKeyData
--                  | RenderKeyUp RenderKeyData
--                  | RenderMouseDown (Int,Int) RenderMouseButton
--                  | RenderMouseUp (Int,Int) RenderMouseButton
--                  | RenderQuit
--                  | RenderNullEvent
--                  | RenderUnknownEvent
--                  deriving (Show)
-- data RenderKeyData = RenderKeyData { renderKeyDataKey :: Int
--                                    , renderKeyDataModifiers :: [RenderKeyModifier]
--                                    } deriving (Show)

-- data RenderKeyModifier = RenderKeyShift
--                        | RenderKeyAlt
--                        | RenderKeyCtrl
--                        deriving (Show)

-- data RenderMouseButton = RenderMouseLeft
--                        | RenderMouseMiddle
--                        | RenderMouseRight
--                        | RenderMouseWheelUp
--                        | RenderMouseWheelDown
--                        | RenderMouseButtonUnknown
--                        deriving (Show)



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

