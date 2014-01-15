module Utils.Output.GNUPlot where

class GNUPlottable a where
  gnuplot :: a -> String
