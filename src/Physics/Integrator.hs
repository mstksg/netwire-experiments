{-# LANGUAGE Arrows #-}

module Physics.Integrator
  ( Integrator(..)
  , runIntegratorPos
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
-- vector and velocity vector integrated from the given initial position
-- and velocity.
--
newtype Integrator =
  Integrator { runIntegrator :: forall v t e a m s.
    (Monad m, Monoid e, HasTime t s, Additive v, Fractional (v a), Fractional a)
    => v a                    -- Initial position
    -> v a                    -- Initial velocity
    -> Wire s e m (v a) (v a, v a)
  }

runIntegratorPos ::
    (Monad m, Monoid e, HasTime t s, Additive v, Fractional (v a), Fractional a)
    => Integrator
    -> v a
    -> v a
    -> Wire s e m (v a) (v a)
runIntegratorPos igr x0 v0 = fst <$> runIntegrator igr x0 v0

-- | Euler integrator
--
euler :: Integrator
euler = Integrator $
  \x0 v0 -> proc acc -> do
    vel <- integral v0 -< acc
    pos <- integral x0 -< vel
    returnA -< (pos, vel)

-- | Verlet integrator
--
verlet :: Integrator
verlet = Integrator vVel
  where
    vVel x0 v0 = snd <$> vInt
      where
        vInt = mkSF wpure
        wpure ds _ = (tup, loop' ds tup)
          where
            tup = (x0 ^-^ (v0 ^* dt), (x0, v0))
            dt = realToFrac $ dtime ds
        loop' ds1 (x1, (x2, _)) = mkSF $ \ds2 a ->
            let dt1 = realToFrac $ dtime ds1
                dt2 = realToFrac $ dtime ds2
                dtr = dt2 / dt1
                x3  = (x2 ^+^ (x2 ^-^ x1) ^* dtr) ^+^ (a ^* (dt2 * dt2))
                v   = x3 ^-^ x2
                tup' = (x2, (x3, v))
            in  (tup', loop' ds1 tup')

-- -- | Runge-Kutta (RK4) integrator
-- --
-- rk4 :: Integrator
-- rk4 = undefined
