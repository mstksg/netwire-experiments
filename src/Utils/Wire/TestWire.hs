module Utils.Wire.TestWire (testWire', testWireRight) where

import Prelude hiding              ((.), id)
import Control.Monad.Writer.Strict
import Control.Wire
-- import FRP.Netwire

-- | Test a wire with a constant timestep and execute a monadic action
-- based on the result at every step.  The action must be of type `Either
-- e b -> m ()` to handle the error case.  Use testWireRight to just have
-- use simple `b -> m ()` and ignore all error cases.
--
testWire' :: forall e t m b. (Monoid e, Real t, Monad m, Applicative m)
    => Int                          -- Number of iterations
    -> t                            -- timestep
    -> (Either e b -> m ())         -- action to run
    -> Wire (Timed t ()) e m () b   -- Wire to test
    -> m ()
testWire' n dt action = go n (countSession dt <*> pure ())
  where
    go i s' w'  | i <= 0 = return ()
                | otherwise = do
      (ds, s) <- stepSession s'
      (mx, w) <- stepWire w' ds (Right ())
      action mx
      go (i-1) s w

-- | Test a wire with a constant timestep and execute a monadic action `b
-- -> m () based on the result at every step, whenever the step is not an
-- error.
--
testWireRight :: forall e t m b. (Monoid e, Real t, Monad m, Applicative m)
    => Int                          -- Number of iterations
    -> t                            -- timestep
    -> (b -> m ())                  -- action to run
    -> Wire (Timed t ()) e m () b   -- Wire to test
    -> m ()
testWireRight n dt action = testWire' n dt action'
  where
    action' = either (const (return ())) action
