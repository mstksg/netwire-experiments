module Render.Backend.SDL where

import Control.Wire
import Linear.Vector
import Data.Bits
import Data.Word
import Render.Render
import Linear.V2
import qualified Graphics.UI.SDL           as SDL
import qualified Graphics.UI.SDL.Framerate as Framerate

sdlBackend :: SDLRenderable a
    => Int
    -> Int
    -> (Word8, Word8, Word8)
    -> Backend (Timed NominalDiffTime ()) e IO (SDL.Surface -> IO ()) a
sdlBackend ht wd (cr,cg,cb) = Backend runGnuplot
  where
    fr = 120
    simDt = 2/60
    runGnuplot r wr = SDL.withInit [SDL.InitEverything] $ do
      screen <- SDL.setVideoMode ht wd 32 [SDL.SWSurface]--, SDL.Fullscreen]

      frameRate <- Framerate.new
      Framerate.init frameRate
      Framerate.set frameRate fr

      -- go screen clockSession_ wr frameRate
      go screen (countSession_ simDt) wr frameRate

      where
        go screen s' w' frameRate = do

          (ds, s) <- stepSession s'
          (mx, w) <- stepWire w' ds (Right ())

          case mx of
            Right mx' -> do

              Framerate.delay frameRate

              pix <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) cr cg cb
              SDL.fillRect screen Nothing pix

              renderSDL mx' zero 1 screen
              r mx' screen

              SDL.flip screen

              go screen s w frameRate

            Left _ -> return ()


class SDLRenderable a where
  renderSDL ::
         a            -- item to render
      -> V2 Double    -- origin
      -> Double       -- scale
      -> SDL.Surface  -- surface
      -> IO ()

instance SDLRenderable a => SDLRenderable [a] where
  renderSDL xs origin scale s = mapM_ (\x -> renderSDL x origin scale s) xs

rgbColor :: Word8 -> Word8 -> Word8 -> SDL.Pixel
rgbColor r g b = SDL.Pixel (shiftL (fi r) 24 .|.
                            shiftL (fi g) 16 .|.
                            shiftL (fi b) 8  .|.
                            255)
  where fi = fromIntegral
