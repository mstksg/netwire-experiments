module Render.Backend.GNUPlot where

import Control.Wire
import Prelude hiding ((.),id)
import Render.Render
import Utils.Wire.TestWire

gnuPlotBackend ::
       Real t
    => t
    -> Int
    -> Backend (Timed t ()) String IO (IO ()) a
gnuPlotBackend dt n = Backend runGnuplot
  where
    runGnuplot r wr = testWire' n dt (either print r) (wr . never)

class GNUPlottable a where
  gnuplot :: a -> String

