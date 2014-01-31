{-# OPTIONS -fno-warn-orphans #-}

module Experiment.Planets.Instances.GNUPlot where

import Render.Backend.GNUPlot
import Experiment.Planets.Types

instance GNUPlottable Planet where
  gnuplot (Planet _ _ _ b) = gnuplot b

