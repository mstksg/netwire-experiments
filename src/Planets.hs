{-# LANGUAGE Arrows #-}

-- import Control.Monad             (void)
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
import qualified Data.Traversable   as Tr

type V3D = V3 Double

data Body = Body Double !V3D
  deriving (Show)

instance GNUPlottable Body where
  gnuplot (Body _ (V3 x y z)) = unwords . map show $ [x,y,z]

processPlanetData :: String -> [(Body, V3D)]
processPlanetData = map processLine . lines
  where
    processLine = makeData . map read . drop 1 . words
    makeData (m:px:py:pz:vx:vy:vz:_) = (Body m (V3 py px pz), V3 vx vy vz)

main :: IO ()
main = do
  planetData <- processPlanetData <$> readFile "data/planet_data.dat"
  let
    logs = execWriter $ testWireRight
      100
      (0.02 :: Double)
      (tell . return)
      (manyBody planetData verlet :: (MonadFix m, HasTime t s) => Wire s String m () [Body])
  writeLogs logs 10

writeLogs :: [[Body]] -> Int -> IO ()
writeLogs logs n = forM_ [0..(n-1)] $ \i ->
  writeFile
    ("out/planets_b" ++ show i ++ ".dat")
    (unlines . map (gnuplot . (!! i)) $ logs)



  -- writeFile "out/planets_b0.dat" . unlines . map (gnuplot . (!! 0)) $ logs
  -- writeFile "out/planets_b1.dat" . unlines . map (gnuplot . (!! 1)) $ logs
  -- writeFile "out/planets_b2.dat" . unlines . map (gnuplot . (!! 2)) $ logs
  -- where
  --   logs = execWriter logWriter
  --   logWriter = testWireRight
  --     50000
  --     (0.02 :: Double)
  --     (tell . return)
  --     (threeBody verlet :: (MonadFix m, HasTime t s) => Wire s String m () [Body])

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
    => Body       -- initial body state
    -> V3D        -- initial velocity
    -> Integrator -- integrator
    -> Wire s e m Body Body
bodyG b0 v0 igr = bGs . arr return
  where
    bGs = bodyGs b0 v0 igr


-- | Wire of a simple body under many gravitational sources
--
bodyGs :: (MonadFix m, Monoid e, HasTime t s)
    => Body       -- initial body state
    -> V3D        -- initial velocity
    -> Integrator -- integrator
    -> Wire s e m [Body] Body
bodyGs b0 v0 igr = proc others -> do
  rec
    let gravs = map (`bodyGravity` b) others
    b <- bF -< gravs
  returnA -< b
  where
    bF = bodyF b0 v0 igr

-- | Wire of a simple body under various forces
--
bodyF :: (MonadFix m, Monoid e, HasTime t s)
    => Body
    -> V3D
    -> Integrator
    -> Wire s e m [V3D] Body
bodyF (Body m x0) v0 igr = thisBody <$>
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
    body1 = bodyG (Body 1000 zero)      zero             igr
    body2 = bodyG (Body 1 (V3 50 0 0))  (V3 0.1 1 0.1)   igr

manyBody :: (MonadFix m, Monoid e, HasTime t s)
    => [(Body, V3D)]          -- Initial body states and initial velocities
    -> Integrator             -- Integrator
    -> Wire s e m () [Body]
manyBody bodyList igr = Tr.sequenceA wireList
  where
    bSelects = selects bodyList
    toWire ((b0, v0), others) = bodyGs b0 v0 igr . pure (map fst others)
    wireList = map toWire bSelects

threeBody :: (MonadFix m, Monoid e, HasTime t s)
    => Integrator
    -> Wire s e m () [Body]
threeBody = manyBody bodyList
  where
    body1 = (Body 1000 zero, zero)
    body2 = (Body 0.9 (V3 50 0 0), V3 0.1 1 0.1)
    body3 = (Body 1 (V3 0 60 0), V3 0.9 0.1 0.4)
    bodyList = [body1,body2,body3]

selects :: [a] -> [(a,[a])]
selects = go []
  where
   go xs [] = []
   go xs (y:ys) = (y,xs++ys) : go (y:xs) ys

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
