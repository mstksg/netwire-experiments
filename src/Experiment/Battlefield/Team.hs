module Experiment.Battlefield.Team (teamWire, TeamWire, TeamWire') where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Random
import Control.Wire
import Control.Wire.Unsafe.Event
import Experiment.Battlefield.Soldier
import Experiment.Battlefield.Types
import Linear.V3
import Prelude hiding                 ((.),id)
import Utils.Helpers                  (foldAcrossl)
import Utils.Wire.Wrapped

type TeamWire s e m = Wire s e m (Team, [Base], [SoldierInEvents]) (Team,[SoldierInEvents])
type TeamWire' = TeamWire (Timed Double ()) () Identity

teamWire :: (MonadFix m, Monoid e, HasTime Double s)
    => (Double, Double)
    -> TeamFlag
    -> StdGen
    -> TeamWire s e m
teamWire (w,h) fl gen =
  proc (Team _ others _, bases, messSldrs) -> do
    startSldrs <- now -< map soldierWire sldrs0
    sldrsEs <- dWireBox' ([], NoEvent) -< (startSldrs, zip (repeat others) messSldrs)
    let
      (sldrsArts,outInEvts) = unzip sldrsEs
      (sldrs,arts) = unzip sldrsArts
      (_outEs,inEs) = unzip outInEvts
      arts' = concat arts
      inEs' = foldAcrossl (<>) mempty inEs
    returnA -< (Team fl sldrs arts',inEs')
  where
    (cswd,carc,caxe,clbw,chrs,char) = (9,5,3,3,4,2)
    (sldrs0,_gen') = flip runRand gen $ do
      swds <- replicateM cswd (genSoldier swordsmanClass)
      arcs <- replicateM carc (genSoldier archerClass)
      axes <- replicateM caxe (genSoldier axemanClass)
      lbws <- replicateM clbw (genSoldier longbowmanClass)
      hrss <- replicateM chrs (genSoldier horsemanClass)
      hars <- replicateM char (genSoldier horsearcherClass)
      return $ concat [swds,arcs,axes,lbws,hrss,hars]
    genSoldier :: SoldierClass -> Rand StdGen SoldierData
    genSoldier (SoldierClass bod weap mnt) = do
      x0 <- V3 <$> getRandomR (0,w) <*> getRandomR (0,h) <*> return 0
      g <- getSplit
      return $ SoldierData x0 (Just fl) (SoldierClass bod weap mnt) g

-- genTeam :: (Monoid e, HasTime Double s, MonadFix m)
--   => (Double, Double)               -- stage dimensions
--   -> TeamFlag                       -- team flag
--   -> (Int,Int,Int,Int,Int,Int)      -- Swordsman, Archers, Axemen, Longbowmen, Horsemen, Horse Archers
--   -> Rand StdGen (TeamWire s e m)
-- genTeam (w,h) fl (cswd,carc,caxe,clbw,chrs,char) = do
--   swds <- replicateM cswd (genSoldier MeleeBody Sword Foot)
--   arcs <- replicateM carc (genSoldier RangedBody Bow Foot)
--   axes <- replicateM caxe (genSoldier TankBody Axe Foot)
--   lbws <- replicateM clbw (genSoldier MeleeBody Longbow Foot)
--   hrss <- replicateM chrs (genSoldier MeleeBody Sword Horse)
--   hars <- replicateM char (genSoldier MeleeBody Bow Horse)
--   let
--     datas = concat [swds,arcs,axes,lbws,hrss,hars]
--   return $ teamWire fl datas
--   where
--     genSoldier :: SoldierBody -> Weapon -> Mount -> Rand StdGen SoldierData
--     genSoldier bod weap mnt = do
--       x0 <- V3 <$> getRandomR (0,w) <*> getRandomR (0,h) <*> return 0
--       gen <- getSplit
--       return $ SoldierData x0 (Just fl) bod weap mnt gen
