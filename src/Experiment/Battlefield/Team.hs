module Experiment.Battlefield.Team (teamWire, TeamWire, TeamWire') where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Random
import Debug.Trace
import Control.Wire as W
import Control.Wire.Unsafe.Event
import Data.Traversable
import FRP.Netwire.Move
import Experiment.Battlefield.Soldier
import Experiment.Battlefield.Types
import Linear.V3
import Linear.Vector
import Prelude hiding                 ((.),id)
import Utils.Helpers                  (foldAcrossl)
import Utils.Wire.Debug
import Utils.Wire.Misc
import Utils.Wire.Noise
import Utils.Wire.Wrapped

type TeamWire s e m = Wire s e m (Team, ((TeamInEvents,[Event BaseEvent]),[SoldierInEvents])) ((Team, [Base]),[SoldierInEvents])
type TeamWire' = TeamWire (Timed Double ()) () Identity

teamWire :: (MonadFix m, Monoid e, HasTime Double s)
    => (Double, Double)
    -> TeamFlag
    -> StdGen
    -> TeamWire s e m
teamWire (w,h) fl gen =
  proc (Team _ others _, ((teamEvts,baseEvts),messSldrs)) -> do
    startSldrs <- now -< map soldierWire sldrs0
    pooled <- couple (noisePrim gen) . soldierPool -< 15
    let
      newSolds = processPool undefined <$> pooled
    sldrsEs <- dWireBox' ([], NoEvent) -< (newSolds <> startSldrs, zip (repeat others) messSldrs)
    let
      (sldrsArts,outInEvts) = unzip sldrsEs
      (sldrs,arts) = unzip sldrsArts
      (_outEs,inEs) = unzip outInEvts
      arts' = concat arts
      inEs' = foldAcrossl (<>) mempty inEs
    returnA -< ((Team fl sldrs arts',[]),inEs')
  where
    -- (cswd,carc,caxe,clbw,chrs,char) = (9,5,3,3,4,2)
    processPool (Base pb) (sldrs,g) = zipWith posser sldrs posses
      where
        maxbdist = 25
        (g0,g') = split (mkStdGen g)
        (g1,g2) = split g'
        posses = zipWith pickDisk (randomRs (0,1) g1) (randomRs (0,2*pi) g2)
        pickDisk r th = pb ^+^ maxbdist *^ V3 (sqrt r * cos th) (sqrt r * sin th) 0
        posser sldr pos = soldierWire $ SoldierData pos (Just fl) sldr g0

    classScores = map ((1 /) . classWorth) allClasses
    classWeight = 10 / sum classScores
    cswd:carc:caxe:clbw:chrs:char:_ = map (round . (classWeight *)) classScores
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

soldierPool :: (Monoid e, MonadFix m, HasTime Double s) => Wire s e m Double (Event [SoldierClass])
soldierPool = proc juice -> do
    mconcat <$> zipWires poolWires -< liftA2 (*) juiceDist (pure juice)
  where
    juiceDist = [1/6,1/6,1/6,1/6,1/6,1/6]
    poolWires = map classPool allClasses
    classPool cls = proc juice -> do
      poolTot <- integral 0 -< juice / 6
      rec
        depletion <- (hold . accumE (+) 0 <|> pure 0) . delay NoEvent -< deplete
        deplete   <- watchDeplete score . delay 0 -< pool
        let
          pool = poolTot - depletion
      let
        popped = [cls] <$ deplete
      returnA -< popped
      where
        watchDeplete lim = (never . W.when (< lim) --> now . W.for 0.1) --> watchDeplete lim
        score = classWorth cls

