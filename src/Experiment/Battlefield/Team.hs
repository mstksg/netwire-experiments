module Experiment.Battlefield.Team (teamWire, teamWireDelayer, TeamWire, TeamWire') where

-- import Control.Monad
-- import Data.Traversable
-- import Debug.Trace
-- import Utils.Wire.Debug
import Control.Monad.Fix
import Control.Monad.Random
import Control.Wire                   as W
import Control.Wire.Unsafe.Event
import Data.Default
import Data.Maybe                     (mapMaybe)
import Experiment.Battlefield.Soldier
import Experiment.Battlefield.Types
import FRP.Netwire.Move
import FRP.Netwire.Noise
import Linear.V3
import Linear.Vector
import Prelude hiding                 ((.),id)
import Utils.Helpers                  (foldAcrossl)
import Utils.Wire.Misc
import Utils.Wire.Noise
import Utils.Wire.Wrapped

type TeamWireIn = (Team, ((TeamInEvents,[BaseEvents]),[SoldierInEvents]))
type TeamWire s e m = Wire s e m TeamWireIn (Team,[SoldierInEvents])
type TeamWire' = TeamWire (Timed Double ()) () Identity

teamWireDelayer :: TeamWireIn
teamWireDelayer = (def,((NoEvent,repeat NoEvent),repeat NoEvent))

teamWire :: (MonadFix m, Monoid e, HasTime Double s)
    => TeamData
    -> TeamWire s e m
teamWire (TeamData fl gen) =
  proc (Team _ others _ _, ((teamEvts,baseEvts),messSldrs)) -> do

    teamEvtsRand <- couple (noisePrim gen) -< teamEvts

    let newBases = newBaseEs <$> teamEvtsRand

    rec
      juice <- (juiceStream . W.when not <|> pure 0) . delay False -< maxedSoldiers

      basesNewSolds <- dWireBox' (0,NoEvent) -< (newBases, zip (repeat juice) baseEvts)

      let (bases,newSolds) = unzip basesNewSolds
          maxSoldiers      = length bases * baseSupply
          newSolds'        = map soldierWire <$> mconcat newSolds

      sldrsEs <- dWireBox' ([], NoEvent) -< (newSolds', zip (repeat others) messSldrs)

      let sldrCount     = length sldrsEs
          maxedSoldiers = sldrCount >= maxSoldiers


    let (sldrsArts,outInEvts) = unzip sldrsEs
        (sldrs,arts)          = unzip sldrsArts
        (_outEs,inEs)         = unzip outInEvts
        arts'                 = concat arts
        inEs'                 = foldAcrossl (<>) mempty inEs

    returnA -< ((Team fl sldrs arts' bases),inEs')

  where
    juiceStream = (pure 300 . W.for 1) --> pure 15
    newBaseEs (evts,g) = zipWith (baseWire fl) gens (mapMaybe getBase evts)
      where
        gens :: [StdGen]
        gens = map mkStdGen $ randoms (mkStdGen g)
        getBase (GotBase b) = Just b

    -- (cswd,carc,caxe,clbw,chrs,char) = (9,5,3,3,4,2)
    -- classScores = map ((1 /) . classWorth) allClasses
    -- classWeight = 10 / sum classScores
    -- cswd:carc:caxe:clbw:chrs:char:_ = map (round . (classWeight *)) classScores
    -- (sldrs0,_gen') = flip runRand gen $ do
    --   swds <- replicateM cswd (genSoldier swordsmanClass)
    --   arcs <- replicateM carc (genSoldier archerClass)
    --   axes <- replicateM caxe (genSoldier axemanClass)
    --   lbws <- replicateM clbw (genSoldier longbowmanClass)
    --   hrss <- replicateM chrs (genSoldier horsemanClass)
    --   hars <- replicateM char (genSoldier horsearcherClass)
    --   return $ concat [swds,arcs,axes,lbws,hrss,hars]
    -- genSoldier :: SoldierClass -> Rand StdGen SoldierData
    -- genSoldier (SoldierClass bod weap mnt) = do
    --   x0 <- V3 <$> getRandomR (0,w) <*> getRandomR (0,h) <*> return 0
    --   g <- getSplit
    --   return $ SoldierData x0 (Just fl) (SoldierClass bod weap mnt) g

baseSupply :: Int
baseSupply = 8

baseWire :: (MonadFix m, Monoid e, HasTime Double s) => TeamFlag -> StdGen -> Base -> Wire s e m (Double, BaseEvents) (Base, Event [SoldierData])
baseWire fl gen b = proc (juice,es) -> do
    die <- filterE (not . null) -< filter isLoseBase <$> es
    pooled <- couple (noisePrim genSldr) . soldierPool genPool -< juice
    let
      newSolds = processPool <$> pooled
    W.until -< ((b',newSolds),die)
  where
    b' = b { baseTeamFlag = Just fl }
    (genSldr,genPool) = split gen
    isLoseBase LoseBase = True
    processPool (sldrs,g) = zipWith posser sldrs posses
      where
        (g0,g') = split (mkStdGen g)
        (g1,g2) = split g'
        posses = zipWith pickDisk (randomRs (0,1) g1) (randomRs (0,2*pi) g2)
        pickDisk r th = (basePos b) ^+^ (baseRadius *^ V3 (sqrt r * cos th) (sqrt r * sin th) 0)
        posser sldr pos = SoldierData pos (Just fl) sldr g0


soldierPool :: (Monoid e, MonadFix m, HasTime Double s) => StdGen -> Wire s e m Double (Event [SoldierClass])
soldierPool gen = proc juice -> do
    juiceDist <- (normDist . mkStdGen <$> hold . noise 1 genDist) <|> pure initDist -< ()
    mconcat <$> zipWires poolWires -< liftA2 (*) juiceDist (pure juice)
  where
    (genInit,genDist) = split gen
    -- defDist = [1/6,1/6,1/6,1/6,1/6,1/6]
    initDist = normDist genInit
    normDist g = map (/ summed) unNormed
      where
        unNormed = take 6 . map (+ 3) $ randomRs (0,1) g
        summed = sum unNormed
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

