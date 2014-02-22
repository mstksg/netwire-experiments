module Utils.Wire.Move where

import Control.Wire
import Control.Wire.Unsafe.Event

decay ::
    (Floating a, HasTime t s)
    => t  -- ^ decay constant lambda
    -> a  -- ^ initial
    -> Wire s e m (Event a) a
decay l x' =
    mkPure $ \ds inp ->
      let curr = case inp of
                   Event dx -> x' + dx
                   NoEvent  -> x'
          dt = realToFrac (dtime ds)
          l' = realToFrac l
          next = curr * exp (-1 * l' * dt)
      in curr `seq` (Right curr, decay l next)

decayH ::
    (Floating a, HasTime t s, Fractional t)
    => t  -- ^ halflife h
    -> a  -- ^ initial
    -> Wire s e m (Event a) a
decayH h = decay l
  where
    h' = realToFrac h
    l  = realToFrac (log (2 :: Double) / h')
