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
main = do
    writeFile "out/planets_newton.dat" $ unlines (logs newton)
    writeFile "out/planets_verlet.dat" $ unlines (logs verlet)
  where
    logs i = execWriter $ logWriter i
    logWriter i = testWireRight
      10000
      (0.01 :: Double)
      (tell . return)
      (gnuplot <$> body x0 v0 xa i
          :: (Monad m, HasTime t s, MonadFix m, Fractional t)
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
--
body :: (MonadFix m, Monoid e, HasTime t s, Fractional t)
    => V3D  -- initial position
    -> V3D  -- initial velocity
    -> V3D  -- position of attractor
    -> Integrator -- integrator
    -> Wire s e m () Body
body x0 v0 xa igr = Body <$> proc _ -> do
  rec
    acc <- arr (gravity 1 xa 1) -< pos
    -- pos <- delay x0 . integrator newton x0 v0 -< acc
    pos <- delay x0 . integrator igr x0 v0 -< acc
    -- pos <- integrator verlet x0 v0 -< acc
    -- pos <- integrator verlet x0 v0 -< pure 0.01
  returnA -< pos

-- testDerivative :: Wire s e m () Double
-- testDerivative = proc _ -> do
--   rec
--     acc <- integral 0 -< pure 0.1
--     pos <- derivative -< acc
--   returnA -< pos

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

-- | Integrator newtype, wrapping around integrator functions.  Integrator
-- functions in general take an acceleration vector and return a position
-- vector integrated from the given initial position and velocity.
--
newtype Integrator =
  Integrator { integrator :: forall m e t s v a.
    (Monad m, Monoid e, HasTime t s, Additive v, Fractional (v a), Fractional a, Fractional t)
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

-- | Verlet integrator
--
verlet :: Integrator
verlet = Integrator vVel
  where
    vVel x0 v0 = snd <$> vInt
      where
        vInt = mkPure wpure
        wpure ds _ = (Right tup, loop' ds tup)
          where
            tup = (x0 ^-^ (v0 ^* dt), x0)
            dt = realToFrac $ dtime ds
        loop' ds1 (x1, x2) = mkPure $ \ds2 a ->
            let dt1 = realToFrac $ dtime ds1
                dt2 = realToFrac $ dtime ds2
                dtr = dt2 / dt1
                x3  = (x2 ^+^ (x2 ^-^ x1) ^* dtr) ^+^ (a ^* (dt2 * dt2))
                tup' = (x2, x3)
            in  (Right tup', loop' ds1 tup')



-- derivative ::
--     (RealFloat a, HasTime t s, Monoid e)
--     => Wire s e m a a
-- derivative = mkPure $ \_ x -> (Left mempty, loop x)
--     where
--     loop x' =
--         mkPure $ \ds x ->
--             let dt  = realToFrac (dtime ds)
--                 dx  = (x - x') / dt
--                 mdx | isNaN dx      = Right 0
--                     | isInfinite dx = Left mempty
--                     | otherwise     = Right dx
--             in mdx `seq` (mdx, loop x)

