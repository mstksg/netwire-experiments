{-# LANGUAGE Arrows, RankNTypes, ScopedTypeVariables #-}

import Prelude hiding ((.), id)
-- import Control.Monad (void)
import Control.Monad.Writer.Strict
import Control.Category
import Control.Wire
import FRP.Netwire
-- import qualified Graphics.UI.SDL as SDL
-- import Data.Traversable
import Linear.V3
import Utils.Output.GNUPlot
-- import Utils.Wire.LogWire
import Utils.Wire.TestWire
import Linear.Metric
import Linear.Vector

type V3D = V3 Double

data Body = Body V3D
  deriving (Show)

instance GNUPlottable Body where
  gnuplot (Body (V3 x y z)) = unwords . map show $ [x,y,z]

main :: IO ()
main = writeFile "out/planets.dat" $ unlines logs
  where
    logs = execWriter logWriter
    logWriter = testWireRight
      10000
      (0.005 :: Double)
      (tell . return)
      (gnuplot <$> body x0 v0 xa
          :: (Monad m, HasTime t s, MonadFix m)
              => Wire s String m () String)
    x0 = zero
    v0 = V3 1   0 0.05
    xa = V3 0.2 1 0

-- main :: IO ()
-- main = SDL.withInit [SDL.InitEverything] $ do
--   screen <- SDL.setVideoMode 600 600 32 [SDL.SWSurface]
--   -- void $ go undefined screen clockSession planets
--   return ()

 -- where
 --   go = undefined

  -- go keysDown screen s w = do
  --   keysDown' <- parseEvents keysDown
  --   (x, w', s') <- stepSession_ w s keysDown'

  --   (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
  --       SDL.fillRect screen Nothing

  --   (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
  --       SDL.fillRect screen (Just $ SDL.Rect (round x) 0 50 50)

  --   SDL.flip screen
  --   go keysDown' screen s' w'

-- | Wire of a simple body under gravity
body :: (MonadFix m, Monoid e, HasTime t s)
    => V3D  -- initial position
    -> V3D  -- initial velocity
    -> V3D  -- position of attractor
    -> Wire s e m () Body
body x0 v0 xa = Body <$> proc _ -> do
  rec
    acc <- arr (gravity 1 xa 1) -< pos
    pos <- integrator newton x0 v0 -< acc
  returnA -< pos

-- | Force of gravity
--
gravity ::
    (Fractional (v a), Floating a, Metric v, Additive v)
    => a    -- mass of attractor
    -> v a  -- position of attractor
    -> a    -- mass of object
    -> v a  -- position of self
    -> v a  -- magnitude of graviational force
gravity m1 r1 m2 r2 = mag *^ signorm r
  where
    r = r1 ^-^ r2
    mag = m1 * m2 / (norm r ** 2)

-- | Integrator newtype, wrapping around integrator functions.
newtype Integrator =
  Integrator { integrator ::
    (Monad m, Monoid e, HasTime t s, Additive v, Fractional (v a))
    => v a                    -- Initial position
    -> v a                    -- Initial velocity
    -> Wire s e m (v a) (v a)
  }

-- | Newtonian integrator
--
newton :: Integrator
newton = Integrator $
  \x0 v0 -> proc acc -> do
    vel <- integral v0 -< acc
    integral x0 -< vel

