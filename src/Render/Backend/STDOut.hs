module Render.Backend.STDOut where

import Control.Wire
import Prelude hiding ((.),id)
import Render.Render
import Utils.Wire.TestWire

stdOutBackend ::
       (Real t, Monoid e, Show e, Show a)
    => t
    -> Int
    -> Backend (Timed t ()) e IO (IO ()) a
stdOutBackend dt n = Backend runStdOutBackend
  where
    runStdOutBackend r wr = testWire' n dt (either print act) (wr . never)
      where
        act x = print x >> r x
