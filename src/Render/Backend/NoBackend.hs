module Render.Backend.NoBackend where

import Control.Wire
import Prelude hiding ((.),id)
import Render.Render
import Utils.Wire.TestWire

noBackend ::
       (Real t, Monoid e, Show e, Show a)
    => t
    -> Backend (Timed t ()) e IO () a
noBackend dt = Backend runStdOutBackend
  where
    runStdOutBackend _ wr = testWire' maxBound dt (const (return ())) (wr . never)

