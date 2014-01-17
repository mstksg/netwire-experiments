{-# LANGUAGE Arrows #-}

module Physics.Integrator
  ( Integrator(..)
  , euler
  , verlet
  ) where

import Control.Monad.Writer.Strict
import Control.Wire
import FRP.Netwire
import Linear.Vector
import Prelude hiding               ((.), id)



-- | Integrator newtype, wrapping around integrator functions.  Integrator
-- functions in general take an acceleration vector and return a position
-- vector integrated from the given initial position and velocity.
--
newtype Integrator =
  Integrator { runIntegrator :: forall v t e a m s.
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

-- -- | Runge-Kutta (RK4) integrator
-- --
-- rk4 :: Integrator
-- rk4 = undefined
