
module Utils.Wire.Debug where

import Control.Wire.Unsafe.Event
import Debug.Trace

traceEvent' :: Show a => Event a -> b -> b
traceEvent' (Event x) y = traceShow x y
traceEvent' NoEvent y = y

