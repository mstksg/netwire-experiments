{-# LANGUAGE TupleSections #-}

module Experiment.Battlefield.Team (teamWire, teamWireDelayer, TeamWire) where

-- import Control.Monad
-- import Data.Maybe                   (mapMaybe)
-- import Data.Traversable
-- import Debug.Trace
-- import Experiment.Battlefield.Stats
-- import Utils.Wire.Debug
import Control.Monad.Fix
import Control.Monad.Random
import Control.Wire                    as W
import Control.Wire.Unsafe.Event
import Data.Default
import Data.Map.Strict                 ((!))
import Experiment.Battlefield.Soldier
import Experiment.Battlefield.Types
import FRP.Netwire.Move
import FRP.Netwire.Noise
import Linear.V3
import Linear.Vector
import Prelude hiding                  ((.),id)
import Utils.Helpers                   (partition3,zipMapWithDefaults)
import Utils.Wire.Misc
import Utils.Wire.Noise
import Utils.Wire.Wrapped
import qualified Data.Map.Strict       as M

type TeamWireIn = ((Team,[Base]), ([BaseEvents],M.Map UUID SoldierInEvents))
type TeamWireOut = (Team,(M.Map UUID SoldierOutEvents,[Event [Attack]]))
type TeamWire s e m = Wire s e m TeamWireIn TeamWireOut

teamWireDelayer :: [Base] -> TeamWireIn
teamWireDelayer b0s = ((def,b0s),(repeat NoEvent,mempty))

teamWire :: forall s e m. (MonadFix m, HasTime Double s, Monoid e)
    => [Base]
    -> TeamData
    -> TeamWire s e m
teamWire b0s (TeamData fl gen) =
  proc ((Team _ others _,bases), (baseEvts,messSldrs)) -> do

    rec
      juice <- (juiceStream . W.when (not . fst) <|> pure 0) . delay (False, numBases) -< (maxedSoldiers, length ownedB)

      let baseSwappers = zipWith baseSwapper' (zip bases gens) baseEvts

      basesNewSolds <- zipWires (zipWith baseSwitcher b0s bgens) . delay (repeat baseDelay) -< zip (repeat juice) baseSwappers


      let (gens,newSolds) = unzip basesNewSolds
          ((ownedB,enemyB),neutB) = partition3 selectBases bases
          -- targetBases | null neutB = enemyB
          --             | otherwise  = neutB
          targetBases | attackPhase = neutB ++ enemyB
                      | otherwise   = neutB ++ enemyB ++ ownedB
          -- maxSoldiers          = round (fromIntegral (length ownedB) * baseSupply')
          maxSoldiers          = totalSupply
          newSolds'            = map soldierWire <$> mconcat newSolds
          sldrs = fst <$> sldrsEs
          sldrIns = (others,targetBases) <$ sldrs
          sldrInsMsgs = zipMapWithDefaults (,) (Just (others,targetBases)) (Just NoEvent) sldrIns messSldrs


      attackPhase <- phaseWire . delay 0.5 -< soldierCapacity

      sldrsEs <- dWireMap ((mempty,[]), NoEvent) uuids -< (newSolds', sldrInsMsgs)

      let sldrCount       = M.size sldrsEs
          soldierCapacity = fromIntegral sldrCount / fromIntegral maxSoldiers
          maxedSoldiers   = sldrCount >= maxSoldiers
          buildings       = undefined


    let outEvts = snd <$> sldrsEs

    returnA -< ((Team fl sldrs buildings),(outEvts,repeat NoEvent))

  where
    totalSupply  = 20
    juiceLimit   = 2.5
    numBases     = length b0s
    juiceAmount  = juiceLimit / fromIntegral numBases
    reserveJuice = 0.7
    (bgen,_g')   = split gen
    bgens        = map mkStdGen (randoms bgen)
    juiceStream  = (pure 25 . W.for 0.5) --> arr ((juiceAmount +) . (reserveJuice /) . fromIntegral . max 1 . snd)
    -- baseSupply' = fromIntegral totalSupply / fromIntegral (length b0s)
    -- selectBases = fmap (== fl) . baseTeamFlag
    selectBases (Base _ Nothing _ _ _) = Nothing
    selectBases (Base _ (Just bfl) sec _ _) | bfl /= fl = Just False
                                            | sec < 0.8 = Nothing
                                            | otherwise = Just True
    baseSwapper' :: (Base,StdGen) -> BaseEvents -> Event (Wire s e m  Double (StdGen, Event [SoldierData]))
    baseSwapper' bg (Event xs@(_:_)) = Event $ baseSwapper bg (last xs)
    baseSwapper' bg (Event []) = Event $ baseSwapper bg (LoseBase Nothing)
    baseSwapper' _ NoEvent = NoEvent

    baseDelay = (0,NoEvent)

    baseSwapper (base,g) GetBase = baseWire fl g base
    baseSwapper (_,g) (LoseBase _) = pure (g, NoEvent)

    baseSwitcher base g = drSwitch w0
      where
        bfl = baseTeamFlag base
        w0 =
          case bfl of
            Just bfl' | bfl' == fl -> baseSwapper (base,g) GetBase
            _                      -> baseSwapper (base,g) (LoseBase bfl)

    phaseWire :: Wire s e m Double Bool
    phaseWire = initialPhase --> phaseLoop
      where
        phaseLoop = buildPhase --> attackPhase --> phaseLoop
        initialPhase = pure True . W.for 30
        buildPhase  = pure False . W.when (< 0.95)
        attackPhase = pure True . W.when (> 0.85)
        -- attackPhase = pure True . W.for 15
        -- attackPhase = pure True . (W.for 15 <|> W.when (> 0.85))

    -- newBaseEs (evts,g) = zipWith (baseWire fl) gens (mapMaybe getBase evts)
    --   where
    --     gens :: [StdGen]
    --     gens = map mkStdGen $ randoms (mkStdGen g)
    --     -- getBase (GotBase b) = Just b
    --     getBase _ = Nothing

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

baseWire :: (MonadFix m, Monoid e, HasTime Double s)
    => TeamFlag
    -> StdGen
    -> Base
    -> Wire s e m Double (StdGen, Event [SoldierData])
baseWire fl gen b = proc juice -> do
    pooled <- couple (noisePrim genSldr) . soldierPool genPool -< juice
    let
      newSolds = processPool <$> pooled
    returnA -< (g11,newSolds)
  where
    (g00,genSldr) = split gen
    (genPool,g11) = split g00
    processPool (sldrs,g) = zipWith posser sldrs posses
      where
        (g0,g') = split (mkStdGen g)
        (g1,g2) = split g'
        posses = zipWith pickDisk (randomRs (0,1) g1) (randomRs (0,2*pi) g2)
        pickDisk r th = (basePos b) ^+^ ((baseRadius * 1.25) *^ V3 (sqrt r * cos th) (sqrt r * sin th) 0)
        posser sldr pos = SoldierData pos (Just fl) sldr g0


soldierPool :: (Monoid e, MonadFix m, HasTime Double s)
    => StdGen
    -> Wire s e m Double (Event [SoldierClass])
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

      juiced <- integral 0 -< juice

      rec
        depletion <- (hold . accumE (+) 0 <|> pure 0) . delay NoEvent -< deplete
        deplete   <- watchDeplete score . delay 0 -< juiced - depletion

      let
        popped = [cls] <$ deplete
      returnA -< popped
      where
        watchDeplete lim = (never . W.when (< lim) --> now . W.for 0.1) --> watchDeplete lim
        score = (normedClassWorths ! cls) * 10

