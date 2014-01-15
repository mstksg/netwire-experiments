{-# LANGUAGE Arrows #-}

-- import Control.Monad             (void)
-- import Data.Traversable
-- import Utils.Wire.LogWire
-- import qualified Graphics.UI.SDL as SDL
import Control.Category
import Control.Monad.Writer.Strict
import Control.Wire
import FRP.Netwire
import Linear.Metric
import Linear.V3
import Linear.Vector
import Prelude hiding               ((.), id)
import Utils.Output.GNUPlot
import Utils.Wire.TestWire

type V3D = V3 Double

data Body = Body Double V3D
  deriving (Show)

instance GNUPlottable Body where
  gnuplot (Body _ (V3 x y z)) = unwords . map show $ [x,y,z]

main :: IO ()
main = do
  writeFile "out/planets_b1.dat" . unlines . map (gnuplot . fst) $ logs
  writeFile "out/planets_b2.dat" . unlines . map (gnuplot . snd) $ logs
  where
    logs = execWriter logWriter
    logWriter = testWireRight
      10000
      (0.003 :: Double)
      (tell . return)
      (twoBody verlet :: (MonadFix m, HasTime t s) => Wire s String m () (Body, Body))

    -- writeFile "out/planets_newton.dat" $ unlines (logs euler)
    -- writeFile "out/planets_verlet.dat" $ unlines (logs verlet)
  -- where
    -- logs i = execWriter $ logWriter i
    -- logWriter i = testWireRight
    --   10000
    --   (0.01 :: Double)
    --   (tell . return)
    --   (gnuplot <$> body x0 v0 xa i
    --       :: (HasTime t s, MonadFix m)
    --           => Wire s String m () String)
    -- x0 = zero
    -- v0 = V3 1   0 0.05
    -- xa = V3 0.2 1 0

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

-- | Wire of a simple body under a single varying gravity source
--
bodyG :: (MonadFix m, Monoid e, HasTime t s)
    => Double     -- mass
    -> V3D        -- initial position
    -> V3D        -- initial velocity
    -> Integrator -- integrator
    -> Wire s e m Body Body
bodyG m x0 v0 igr = bGs . arr return
  where
    bGs = bodyGs m x0 v0 igr


-- | Wire of a simple body under many gravitational sources
--
bodyGs :: (MonadFix m, Monoid e, HasTime t s)
    => Double     -- mass
    -> V3D        -- initial position
    -> V3D        -- initial velocity
    -> Integrator -- integrator
    -> Wire s e m [Body] Body
bodyGs m x0 v0 igr = proc others -> do
  rec
    let gravs = map (`bodyGravity` b) others
    b <- bF -< gravs
  returnA -< b
  where
    bF = bodyF m x0 v0 igr

-- | Wire of a simple body under various forces
--
bodyF :: (MonadFix m, Monoid e, HasTime t s)
    => Double
    -> V3D
    -> V3D
    -> Integrator
    -> Wire s e m [V3D] Body
bodyF m x0 v0 igr = thisBody <$>
    delay x0 . integrator igr x0 v0 . arr ((^/ m) . sum)
  where
    thisBody = Body m

-- | Two body system
--
twoBody :: (MonadFix m, Monoid e, HasTime t s)
    => Integrator
    -> Wire s e m () (Body,Body)
twoBody igr = proc _ -> do
  rec
    b1 <- body1 -< b2
    b2 <- body2 -< b1
  returnA -< (b1,b2)
  where
    body1 = bodyG 1000 zero         zero             igr
    body2 = bodyG 1    (V3 50 0 0)  (V3 0.1 1 0.1)   igr

-- manyBodies :: (MonadFix m, Monoid e, HasTime t s)
--     => Integrator
--     -> Wire s e m () [Body]
-- manyBodies igr = proc _ -> do
--   rec
--     bInteracts -<
--     bList <-
--   where
--     body1 = bodyGs 1000 zero zero igr
--     body2 = bodyGs 1 (V3 50 0 0) (V3 0.1 1 0.1) igr
--     body3 = bodyGs 1 (V3 0 50 0) (V3 0.9 0.1 0.5) igr
--     bodyList = [body1,body2,body3]
--     bodyInteractions = selects bodyList

-- selects :: [a] -> [(a,[a])]
-- selects xs0 = go [] xs0
--   where
--    go xs [] = []
--    go xs (y:ys) = (y,xs++ys) : go (y:xs) ys

-- | Force of gravity between bodies
--
bodyGravity ::
       Body -- attractor
    -> Body -- self
    -> V3D  -- gravitational force
bodyGravity (Body m1 r1) (Body m2 r2) = gravity m1 r1 m2 r2

-- | Force of gravity
--
gravity ::
    (Fractional (v a), Floating a, Metric v, Additive v)
    => a    -- mass of attractor
    -> v a  -- position of attractor
    -> a    -- mass of object
    -> v a  -- position of self
    -> v a  -- graviational force
gravity m1 r1 m2 r2 = mag *^ signorm r
  where
    r = r1 ^-^ r2
    mag = m1 * m2 / (norm r ** 2)

-- | Integrator newtype, wrapping around integrator functions.  Integrator
-- functions in general take an acceleration vector and return a position
-- vector integrated from the given initial position and velocity.
--
newtype Integrator =
  Integrator { integrator :: forall v t e a m s.
    (Monad m, Monoid e, HasTime t s, Additive v, Fractional (v a), Fractional a)
    => v a                    -- Initial position
    -> v a                    -- Initial velocity
    -> Wire s e m (v a) (v a)
  }

-- | Euler integrator
--
euler :: Integrator
euler = Integrator $
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

-- | Runge-Kutta (RK4) integrator
--
rk4 :: Integrator
rk4 = undefined
