module Experiment.Battlefield.Team (teamWire, genTeam, TeamWire, TeamWire') where

import Control.Monad.Fix
import Control.Wire
import Control.Wire.Unsafe.Event
import Control.Monad
import Linear.V3
import Data.Maybe (mapMaybe)
import Experiment.Battlefield.Attack
import Experiment.Battlefield.Soldier
import Experiment.Battlefield.Types
import Utils.Wire.Wrapped
import Control.Monad.Random

type TeamWire s e m = Wire s e m (Team, ([SoldierInEvents],[Event ()])) Team
type TeamWire' = TeamWire (Timed Double ()) () Identity

teamWire :: (MonadFix m, Monoid e, HasTime t s, Fractional t)
    => TeamFlag
    -> [SoldierData]
    -> TeamWire s e m
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


genTeam :: (Monoid e, HasTime t s, MonadFix m, Fractional t)
  => (Double, Double)               -- stage dimensions
  -> TeamFlag                       -- team flag
  -> (Int,Int,Int,Int,Int,Int)      -- Swordsman, Archers, Axemen, Longbowmen, Horsemen, Horse Archers
  -> Rand StdGen (TeamWire s e m)
genTeam (w,h) fl (cswd,carc,caxe,clbw,chrs,char) = do
  swds <- replicateM cswd (genSoldier MeleeBody Sword Foot)
  arcs <- replicateM carc (genSoldier RangedBody Bow Foot)
  axes <- replicateM caxe (genSoldier MeleeBody Sword Foot)
  lbws <- replicateM clbw (genSoldier MeleeBody Sword Foot)
  hrss <- replicateM chrs (genSoldier MeleeBody Sword Foot)
  hars <- replicateM char (genSoldier MeleeBody Sword Foot)
  let
    datas = concat [swds,arcs,axes,lbws,hrss,hars]
  return $ teamWire fl datas
  where
    genSoldier :: SoldierBody -> Weapon -> Mount -> Rand StdGen SoldierData
    genSoldier bod weap mnt = do
      x0 <- V3 <$> getRandomR (0,w) <*> getRandomR (0,h) <*> return 0
      gen <- getSplit
      return $ SoldierData x0 (Just fl) bod weap mnt gen
