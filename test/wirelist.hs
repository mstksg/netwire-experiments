{-# LANGUAGE Arrows #-}

-- import Control.Monad             (void)
-- import Utils.Wire.LogWire
-- import qualified Graphics.UI.SDL as SDL
import Control.Category
import Control.Monad.Writer.Strict
import Control.Wire
import FRP.Netwire
import Linear.Metric
import Linear.V3
import Linear.Vector
import Prelude hiding               ((.), id)
import Utils.Output.GNUPlot
import Utils.Wire.TestWire
import qualified Data.Traversable   as Tr

arrList :: Arrow r => r [a] [a]
arrList = proc cList -> do
    let
      toArr a = oneArr . pure a
      wireList = map toArr cList
    returnA -< Tr.sequenceA wireList

oneArr :: Arrow r => r a a
oneArr = id

main = return ()
