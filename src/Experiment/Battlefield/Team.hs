module Experiment.Battlefield.Team where

import Control.Monad.Fix
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Maybe (mapMaybe)
import Experiment.Battlefield.Attack
import Experiment.Battlefield.Soldier
import Experiment.Battlefield.Types
import Utils.Wire.Wrapped


teamWire :: (MonadFix m, Monoid e, HasTime t s, Fractional t)
    => TeamFlag
    -> [SoldierData]
    -> Wire s e m (Team, ([SoldierInEvents],[Event ()])) Team
teamWire fl sldrs0 =
  proc (Team _ others _,(messSldrs,messAtks)) -> do
    startSldrs <- now -< map soldierWire sldrs0
    sldrsEs <- dWireBox' ([], NoEvent) -< (startSldrs, zip (repeat others) messSldrs)
    let
      sldrs = map fst sldrsEs
      outEvts = mconcat (map snd sldrsEs)
      newAtks = mapMaybe maybeAttack <$> outEvts
      newAtkWires = map attackWire <$> newAtks
    atks <- dWireBox' NoEvent -< (newAtkWires, messAtks)
    returnA -< Team fl sldrs atks
