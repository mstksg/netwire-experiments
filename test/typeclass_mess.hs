{-# LANGUAGE Arrows, RankNTypes, ScopedTypeVariables #-}

import Control.Wire
import FRP.Netwire
import Linear.Vector
import Prelude hiding ((.), id)

newtype Integrator =
  Integrator { integrator ::
    (Monad m, Monoid e, HasTime t s, Additive v, Fractional (v a), Fractional a, Fractional t)
    => v a                    -- Initial position
    -> v a                    -- Initial velocity
    -> Wire s e m (v a) (v a)
  }

-- | Verlet integrator
--
verlet :: Integrator
verlet = Integrator vVel
  where
    vVel :: forall m e t s v a.
        (Monad m, Monoid e, HasTime t s, Additive v, Fractional (v a), Fractional a, Fractional t)
        => v a
        -> v a
        -> Wire s e m (v a) (v a)
    vVel x0 v0 = snd <$> vInt
      where
        vInt :: (Fractional a, Fractional t) => Wire s e m (v a) (v a, v a)
        vInt = mkPure wpure
        tup :: (v a, v a)
        tup = (x0 ^-^ v0, x0)
        wpure :: (HasTime t s, Fractional t, Fractional a)
            => s
            -> v a
            -> (Either e (v a, v a), Wire s e m (v a) (v a, v a))
        wpure ds _ = (Right tup, loop' ds tup)
        loop' :: (HasTime t s, Fractional t, Fractional a)
          => s
          -> (v a, v a)
          -> Wire s e m (v a) (v a, v a)
        loop' ds1 (x1, x2) = mkPure wpure'
          where
            wpure' :: (HasTime t s, Fractional t, Fractional a)
                => s
                -> v a
                -> (Either e (v a, v a), Wire s e m (v a) (v a, v a))
            wpure' ds2 a =
              let dt1 = realToFrac $ dtime ds1
                  dt2 = realToFrac $ dtime ds2
                  dtr = dt1 / dt2
                  x3  = (x2 ^+^ (x2 ^-^ x1) ^* dtr) ^+^ (a ^* (dt2 * dt2))
                  tup' = (x2, x3)
              in  (Right tup', loop' ds1 tup')

-- | Newtonian integrator
--
newton :: Integrator
newton = Integrator $
  \x0 v0 -> proc acc -> do
    vel <- integral v0 -< acc
    integral x0 -< vel

main = return ()
