
module Render.Backend.GLUT where

import Control.Monad             as M
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Char
import Data.Colour.SRGB          as C
import Data.IORef
import Data.Time.Clock
import Data.Word
import Graphics.UI.GLUT          as GLUT
import Linear.V2
import Linear.Vector
import Prelude hiding            ((.),id)
import Render.Render
import Render.Sprite             as Sprite
import Render.Surface
import qualified Data.Sequence   as S

data GLUTStateRefs e a = GLUTStateRefs
                           { glutStateA        :: IORef (Maybe (Either e a))
                           , glutStateWire     :: IORef (Wire (Timed Double ()) e Identity (Event RenderEvent) a)
                           , glutStateSession  :: IORef (Session Identity (Timed Double ()))
                           , glutStateEQueue   :: IORef (S.Seq RenderEvent)
                           , glutStateStatus   :: IORef String
                           , glutStateLastTime :: IORef UTCTime
                           }


iterationLimit :: Int
iterationLimit = 5

glutBackend :: forall e a. GLUTRenderable a
    => Double                 -- simulation dt
    -> Double                 -- simulation time/real time ratio
    -> (Int,Int)              -- width and height
    -> (Word8, Word8, Word8)  -- background color
    -> Backend (Timed Double ()) e Identity (IO ()) a
glutBackend simDt tScale (wd,ht) (cr,cg,cb) = Backend runGLUT
  where
    sess :: Session Identity (Timed Double ())
    sess = countSession simDt <*> pure ()

    iLimit :: Int
    iLimit = round (tScale * fromIntegral iterationLimit)

    col = Color4
            (fromIntegral cr / 255)
            (fromIntegral cg / 255)
            (fromIntegral cb / 255)
            1

    runGLUT r wr = do
        (progName,_) <- getArgsAndInitialize

        stateRefs <- initializeState

        initialWindowSize $= Size (fromIntegral wd) (fromIntegral ht)
        initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
        createWindow progName
        clearColor $= col
        displayCallback $= display stateRefs
        idleCallback $= Just (step stateRefs)
        keyboardMouseCallback $= Just (keyboardMouse stateRefs)

        mainLoop

      where
        initializeState :: IO (GLUTStateRefs e a)
        initializeState = do
          a          <- newIORef Nothing
          wire       <- newIORef wr
          session    <- newIORef sess
          eventQueue <- newIORef mempty
          status     <- newIORef mempty
          lastTime   <- getCurrentTime >>= newIORef

          return $ GLUTStateRefs  a
                                  wire
                                  session
                                  eventQueue
                                  status
                                  lastTime


        display :: GLUTStateRefs e a -> DisplayCallback
        display (GLUTStateRefs a _ _ _ status _) = do
          mx <- readIORef a

          case mx of
            Nothing -> return ()
            Just (Right mx') -> do
              clear [ColorBuffer]
              renderGLUT mx'

              stat <- readIORef status
              currentRasterPosition $= Vertex4 (-0.95) 0.95 0 1
              renderString Fixed8By13 stat

              r mx'
              swapBuffers
            Just (Left _) -> exit

        step :: GLUTStateRefs e a -> IdleCallback
        step (GLUTStateRefs a wire session evs status lastTime) = do
          s'' <- readIORef session
          w'' <- readIORef wire

          lastT <- readIORef lastTime
          nowT <- getCurrentTime

          let
            tdiff = realToFrac (nowT `diffUTCTime` lastT)
            iters' = max 0 (floor (tScale * tdiff / simDt)) :: Int
            iters = min iLimit iters'
            newLast = realToFrac (simDt * fromIntegral iters' / tScale) `addUTCTime` lastT

          -- writeIORef status ((show . (round :: Double -> Int)) (1/tdiff))

          let
            (s',w') = stepN (iters-1) (s'',w'')

          M.when (iters > 0) $ do
            let
              (ds,s) = runIdentity (stepSession s')

            evseq <- readIORef evs
            ev <- case S.viewl evseq of
                    S.EmptyL        ->
                      return NoEvent
                    ev' S.:< evseq' -> do
                      writeIORef evs evseq'
                      return (Event ev')

            case ev of
              NoEvent -> return ()
              Event e -> writeIORef status (show e)

            let
              (mx,w) = runIdentity (stepWire w' ds (Right ev))

            writeIORef wire w
            writeIORef session s
            writeIORef a (Just mx)

          writeIORef lastTime newLast

          postRedisplay Nothing

        stepN ::
               Int
            -> (Session Identity (Timed Double ()),Wire (Timed Double ()) e Identity (Event RenderEvent) a)
            -> (Session Identity (Timed Double ()),Wire (Timed Double ()) e Identity (Event RenderEvent) a)
        stepN n (s',w')
          | n <= 0    = (s',w')
          | otherwise = runIdentity $ do
              (ds,s) <- stepSession s'
              (_,w) <- stepWire w' ds (Right NoEvent)
              return $ stepN (n-1) (s,w)


        keyboardMouse :: GLUTStateRefs e a -> KeyboardMouseCallback
        keyboardMouse (GLUTStateRefs _ _ _ evs _ _) key kstate mods (Position x y) =
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
                  in  upDown (RenderMouseDown,RenderMouseUp) pos b'
                SpecialKey _ -> RenderUnknownEvent
                GLUT.Char c ->
                  let mods' = zip
                                (map ($ mods) [shift,alt,ctrl])
                                [RenderKeyShift ..]
                      modList = map snd . filter ((== Down) . fst) $ mods'
                      kData = RenderKeyData (ord c) modList
                  in  upDown (RenderKeyDown,RenderKeyUp) kData


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
  renderGLUT s@(Sprite o sh colour _) =
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
      C.RGB cr cg cb = toSRGB colour
      col :: Color3 GLfloat
      col = Color3
              (realToFrac cr)
              (realToFrac cg)
              (realToFrac cb)

v2ToVertex2 :: Real a => V2 a -> Vertex2 GLfloat
v2ToVertex2 (V2 x y) = Vertex2 (realToFrac x) (realToFrac y)

