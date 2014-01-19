module Render.Backend.GNUPlot where

import Control.Wire
import Render.Render
import Utils.Wire.TestWire

gnuPlotBackend ::
       Real t
    => t
    -> Int
    -> Backend (Timed t ()) String IO (IO ()) a
gnuPlotBackend dt n = Backend runGnuplot
  where
    runGnuplot r = testWire' n dt (either print r)

class GNUPlottable a where
  gnuplot :: a -> String

