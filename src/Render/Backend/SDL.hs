module Render.Backend.SDL where

import Control.Monad
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Bits
import Data.Colour.SRGB
import Data.Maybe                           (mapMaybe)
import Data.Word
import Linear.V2
import Prelude hiding                       ((.),id)
import Render.Render
import Render.Sprite
import Render.Surface
import qualified Data.Map.Strict            as M
import qualified Graphics.UI.SDL            as SDL
import qualified Graphics.UI.SDL.Framerate  as Framerate
import qualified Graphics.UI.SDL.Primitives as SDL
import qualified Graphics.UI.SDL.TTF        as SDLTTF

data RenderFont = FontKA1 deriving (Show, Eq, Ord)

type RenderFontMap = M.Map RenderFont SDLTTF.Font

sdlBackend :: (SDLRenderable a, Show e)
    => Double
    -> Int
    -> (Int,Int)
    -> (Word8, Word8, Word8)
    -> Backend (Timed Double ()) e Identity (SDL.Surface -> RenderFontMap -> IO ()) a
sdlBackend simDt fr (ht,wd) (cr,cg,cb) = Backend runSdl
  where
    runSdl r wr = SDL.withInit [SDL.InitEverything] $ do
      screen <- SDL.setVideoMode ht wd 32 [SDL.SWSurface]--, SDL.Fullscreen]

      frameRate <- Framerate.new
      Framerate.init frameRate
      Framerate.set frameRate fr

      SDLTTF.init
      ka1 <- SDLTTF.openFont "assets/ka1.ttf" 10

      let fontMap = M.fromList [(FontKA1,ka1)]

      -- go screen clockSession_ wr frameRate
      go screen fontMap (countSession simDt <*> pure ()) wr frameRate

      where
        go screen fontMap s' w' frameRate = do

          renderEvent <- processSDLEvent <$> SDL.pollEvent

          -- case renderEvent of
          --   NoEvent -> return ()
          --   Event e -> print e

          let
            (ds, s) = runIdentity (stepSession s')
            (mx, w) = runIdentity (stepWire w' ds (Right renderEvent))

          -- (ds, s) <- runIdentitystepSession s'
          -- (mx, w) <- stepWire w' ds (Right renderEvent)

          case mx of
            Right mx' -> do

              Framerate.delay frameRate

              pix <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) cr cg cb
              SDL.fillRect screen Nothing pix

              r mx' screen fontMap
              renderSDL screen fontMap mx'

              SDL.flip screen

              go screen fontMap s w frameRate

            Left e -> error (show e)

class SDLRenderable a where
  renderSDL ::
         SDL.Surface  -- surface
      -> M.Map RenderFont SDLTTF.Font
      -> a            -- item to render
      -> IO ()


instance SDLRenderable Surface where
  renderSDL scr fm = mapM_ (renderSDL scr fm) . toSpriteList'

instance SDLRenderable Sprite where
  renderSDL scr _ (Sprite (V2 x y) sh colour _) = void (drawer col)
    where
      drawer = case sh of
        Circle r f ->
          let drawFunc = fromFilling f SDL.circle SDL.filledCircle
          in  drawFunc scr (round x) (round y) (round r)
        Ellipse (V2 rx ry) f ->
          let drawFunc = fromFilling f SDL.ellipse SDL.filledEllipse
          in  drawFunc scr (round x) (round y) (round rx) (round ry)
        Rectangle (V2 w h) f ->
          let drawFunc = fromFilling f SDL.rectangle SDL.box
          in  drawFunc scr
                (SDL.Rect
                  (round (x-(w/2)))
                  (round (y-(h/2)))
                  (round (x+(w/2)))
                  (round (y+(h/2))))
        Polygon vs f -> drawFunc scr (map toTup vs)
          where
            toTup (V2 x' y') = (round (x+x'), round (y+y'))
            drawFunc = fromFilling f SDL.polygon SDL.filledPolygon
        Line (V2 x0 y0) (V2 x1 y1)
          | x0 == x1  ->
              SDL.vLine scr (round (x+x0)) (round (y+y0)) (round (y+y1))
          | y0 == y1  ->
              SDL.hLine scr (round (x+x0)) (round (x+x1)) (round (y+y0))
          | otherwise ->
              SDL.line scr
                (round (x+x0)) (round (y+y0))
                (round (x+x1)) (round (y+y1))
      RGB cr cg cb = toSRGB24 colour
      col = rgbColor cr cg cb

rgbColor :: Word8 -> Word8 -> Word8 -> SDL.Pixel
rgbColor r g b = SDL.Pixel (shiftL (fi r) 24 .|.
                            shiftL (fi g) 16 .|.
                            shiftL (fi b) 8  .|.
                            255)
  where fi = fromIntegral

processSDLEvent :: SDL.Event -> Event RenderEvent
processSDLEvent SDL.NoEvent = NoEvent
processSDLEvent (SDL.KeyDown sym) = Event $ RenderKeyDown (sdlKey sym)
processSDLEvent (SDL.KeyUp sym) = Event $ RenderKeyUp (sdlKey sym)
processSDLEvent (SDL.MouseButtonDown x y b) = Event $ RenderMouseDown (fromIntegral x,fromIntegral y) (sdlMouse b)
processSDLEvent (SDL.MouseButtonUp x y b) = Event $ RenderMouseUp (fromIntegral x,fromIntegral y) (sdlMouse b)
processSDLEvent SDL.Quit = Event RenderQuit
processSDLEvent _ = Event RenderUnknownEvent

sdlKey :: SDL.Keysym -> RenderKeyData
sdlKey (SDL.Keysym _ m u) =
  RenderKeyData
    (fromEnum u)
    (mapMaybe sdlModifier m)

sdlMouse :: SDL.MouseButton -> RenderMouseButton
sdlMouse SDL.ButtonLeft = RenderMouseLeft
sdlMouse SDL.ButtonMiddle = RenderMouseMiddle
sdlMouse SDL.ButtonRight = RenderMouseRight
sdlMouse SDL.ButtonWheelUp = RenderMouseWheelUp
sdlMouse SDL.ButtonWheelDown = RenderMouseWheelDown
sdlMouse _ = RenderMouseButtonUnknown

sdlModifier :: SDL.Modifier -> Maybe RenderKeyModifier
sdlModifier SDL.KeyModLeftShift = Just RenderKeyShift
sdlModifier SDL.KeyModRightShift = Just RenderKeyShift
sdlModifier SDL.KeyModLeftAlt = Just RenderKeyAlt
sdlModifier SDL.KeyModRightAlt = Just RenderKeyAlt
sdlModifier _ = Nothing
