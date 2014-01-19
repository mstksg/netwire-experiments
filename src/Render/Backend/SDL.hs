module Render.Backend.SDL where

import Control.Wire
import Render.Render
import qualified Graphics.UI.SDL           as SDL
import qualified Graphics.UI.SDL.Framerate as Framerate
import Data.Word

sdlBackend ::
       Int
    -> Int
    -> (Word8, Word8, Word8)
    -> Backend (Timed NominalDiffTime ()) e IO (SDL.Surface -> IO ()) a
sdlBackend ht wd (cr,cg,cb) = Backend runGnuplot
  where
    runGnuplot r wr = SDL.withInit [SDL.InitEverything] $ do
      screen <- SDL.setVideoMode ht wd 0 [SDL.SWSurface]--, SDL.Fullscreen]

      frameRate <- Framerate.new
      Framerate.init frameRate
      Framerate.set frameRate 120

      go screen clockSession_ wr frameRate

      where
        go screen s' w' frameRate = do

          (ds, s) <- stepSession s'
          (mx, w) <- stepWire w' ds (Right ())

          case mx of
            Right mx' -> do

              Framerate.delay frameRate

              pix <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) cr cg cb
              SDL.fillRect screen Nothing pix

              r mx' screen

              go screen s w frameRate

            Left _ -> return ()


class SDLRenderable a where
  renderSDL :: a -> SDL.Surface -> IO ()

