module Experiment.Battlefield.Stage where

import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Maybe                        (catMaybes)
import Experiment.Battlefield.Team
import Experiment.Battlefield.Types
import Linear.V3
import Prelude hiding                    ((.),id)

stageWire ::
     (Double, Double)
  -> TeamWire'
  -> TeamWire'
  -> Wire' () Stage
stageWire dim@(w,h) t1w t2w = proc _ -> do
    rec
      b10 <- now -< b1s
      b20 <- now -< b2s
      let
        b1es = repeat NoEvent
        b2es = repeat NoEvent
      (team1@(Team _ t1ss t1as t1bs), t2ahits) <- t1w . delay teamWireDelayer -< (team2, ((b10,b1es), t1ahits))
      (team2@(Team _ t2ss t2as t2bs), t1ahits) <- t2w . delay teamWireDelayer -< (team1, ((b20,b2es), t2ahits))
    let
      sldrs = catMaybes (t1ss ++ t2ss)
      arts  = t1as ++ t2as
      bases = t1bs ++ t2bs
    returnA -< Stage dim sldrs arts bases
  where
    b1s = GotBase <$> [Base (V3 (w/6) (h/6) 0) Nothing 1, Base (V3 (w/6) (5*h/6) 0) Nothing 1]
    b2s = GotBase <$> [Base (V3 (5*w/6) (h/6) 0) Nothing 1, Base (V3 (5*w/6) (5*h/6) 0) Nothing 1]


