module Utils.Wire.Interval where

import Control.Wire
import Control.Wire.Unsafe.Event
import Linear.Vector

holdTickAndAmplify :: (HasTime t s, Monoid e, Fractional t, Functor v, Fractional a)
  => Wire s e m (Event (v a)) (v a)
holdTickAndAmplify = off
  where
    off = mkPure $ \_ ->
      event (Left mempty, off)
            (Right &&& on)
    on x = mkPure $ \ds ->
      event (Right (x ^/ realToFrac (dtime ds)), off)
            (Right &&& const off)

