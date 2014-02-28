{-# LANGUAGE TupleSections #-}

module Experiment.Battlefield.Stage
  ( stageWireOnce
  , stageWireLoop
  , stageWireOnce'
  , stageWireLoop'
  ) where

-- import Data.Traversable
import Control.Monad.Fix
import Control.Wire                 as W
import Control.Wire.Unsafe.Event
import Data.Default
import Data.Maybe                   (catMaybes, isNothing)
import Experiment.Battlefield.Stats
import Experiment.Battlefield.Team
import Experiment.Battlefield.Types
import FRP.Netwire.Move
import Linear.Metric
import Linear.V3
import Linear.Vector
import Prelude hiding               ((.),id)
import System.Random
import Utils.Helpers
import Utils.Wire.Misc
import qualified Data.Map.Strict    as M


stageWireOnce :: (MonadFix m, Monoid e, HasTime Double s)
  => (Double,Double)
  -> TeamFlag
  -> TeamFlag
  -> StdGen
  -> Wire s e m () Stage
stageWireOnce dim t1fl t2fl gen = arr fst . stageWireOnce' def dim t1fl t2fl gen

stageWireLoop :: (MonadFix m, Monoid e, HasTime Double s)
  => (Double,Double)
  -> TeamFlag
  -> TeamFlag
  -> StdGen
  -> Wire s e m () Stage
stageWireLoop = stageWireLoop' def

stageWireLoop' :: (MonadFix m, Monoid e, HasTime Double s)
  => StageScore
  -> (Double,Double)
  -> TeamFlag
  -> TeamFlag
  -> StdGen
  -> Wire s e m () Stage
stageWireLoop' stgC dim t1fl t2fl gen = switch stgW
  where
    stgW = second (fmap makeStageWire) <$> stageWireOnce' stgC dim t1fl t2fl g1
    makeStageWire stgC' = stageWireLoop' stgC' dim t1fl t2fl g2
    (g1,g2) = split gen

stageWireOnce' :: (MonadFix m, Monoid e, HasTime Double s)
  => StageScore
  -> (Double, Double)
  -> TeamFlag
  -> TeamFlag
  -> StdGen
  -> Wire s e m () (Stage, Event StageScore)
stageWireOnce' stgC dim@(w,h) t1fl t2fl gen = proc _ -> do

    duration <- integral (stageScoreDuration stgC) -< 1

    rec
      (team1@(Team _ t1ss t1as _), (t2ahits,t2bhits)) <- t1w . delay (teamWireDelayer b0s) -< ((team2,bases), (t1bes, t1ahits))
      (team2@(Team _ t2ss t2as _), (t1ahits,t1bhits)) <- t2w -< ((team1,bases), (t2bes, t2ahits))

      let t1ss' = catMaybes (M.elems t1ss)
          t2ss' = catMaybes (M.elems t2ss)

      (bases,(t1bes,t2bes)) <- basesWire (t1fl,t2fl) b0s . delay (([],[]),repeat NoEvent) -< ((t1ss',t2ss'),zipWith (<>) t2bhits t1bhits)

    let sldrs = t1ss' ++ t2ss'
        arts  = t1as  ++ t2as

    victory <- never . W.when isNothing --> now -< winner bases

    let newData = processVictory duration <$> victory

    returnA -< (Stage dim (stgC { stageScoreDuration = duration }) sldrs arts bases, newData)

  where
    -- t1fl = teamDataFlag t1fl
    -- t2fl = teamDataFlag t2fl
    (t1gen,t2gen) = split gen

    t1w = teamWire b0s $ TeamData t1fl t1gen
    t2w = teamWire b0s $ TeamData t2fl t2gen
    -- b1s = makeBase t1fl <$> [V3 (w/6) (h/6) 0, V3 (w/6) (5*h/6) 0]
    -- b2s = makeBase t2fl <$> [V3 (5*w/6) (h/6) 0, V3 (5*w/6) (5*h/6) 0]
    -- bns = []
    -- b1s = makeBase (Just t1fl) <$> [V3 (w/6) (h/6) 0, V3 (5*w/6) (5*h/6) 0]
    -- b2s = makeBase (Just t2fl) <$> [V3 (5*w/6) (h/6) 0, V3 (w/6) (5*h/6) 0]
    -- b1s = makeBase (Just t1fl) <$> [V3 (w/6) (h/6) 0, V3 (5*w/6) (h/6) 0]
    -- b2s = makeBase (Just t2fl) <$> [V3 (w/6) (5*h/6) 0, V3 (5*w/6) (5*h/6) 0]
    -- bns = makeBase Nothing <$> [V3 (w/2) (h/4) 0, V3 (w/2) (3*h/4) 0, V3 (w/4) (h/2) 0, V3 (3*w/4) (h/2) 0]
    -- bns = makeBase Nothing <$> [V3 (5*w/6) (h/6) 0, V3 (w/2) (h/2) 0, V3 (w/6) (5*h/6) 0]
    -- bns = makeBase Nothing <$> [V3 (w/3) (h/2) 0, V3 (2*w/3) (h/2) 0]
    -- bns = makeBase Nothing <$> [V3 (w/2) (h/2) 0]
    b1s = makeBase (Just t1fl) <$> [V3 (w/6) (h/6) 0]
    b2s = makeBase (Just t2fl) <$> [V3 (5*w/6) (5*h/6) 0]
    -- bns = makeBase Nothing <$> [V3 (w/2) (h/4) 0, V3 (w/2) (3*h/4) 0, V3 (w/4) (h/2) 0, V3 (3*w/4) (h/2) 0, V3 (5*w/6) (h/6) 0, V3 (w/2) (h/2) 0, V3 (w/6) (5*h/6) 0]
    bns = makeBase Nothing <$> [ V3 (w/2) (h/4) 0
                               , V3 (w/2) (3*h/4) 0
                               , V3 (w/2) (h/2) 0
                               -- , V3 (w/3) (3*h/8) 0
                               -- , V3 (w/3) (5*h/8) 0
                               -- , V3 (2*w/3) (3*h/8) 0
                               -- , V3 (2*w/3) (5*h/8) 0
                               , V3 (w/3) (h/3) 0
                               , V3 (w/3) (2*h/3) 0
                               , V3 (2*w/3) (h/3) 0
                               , V3 (2*w/3) (2*h/3) 0
                               , V3 (5*w/6) (h/6) 0
                               , V3 (w/6) (5*h/6) 0
                               ]
    b0s = b1s ++ bns ++ b2s
    makeBase fl pb = Base pb fl 1 Nothing Nothing
    -- numBases = length b0s
    winner bases | null t1bs = Just t2fl
                 | null t2bs = Just t1fl
                 | otherwise = Nothing
      where
        (t1bs,t2bs) = fst $ partition3 (fmap (== t1fl) . baseTeamFlag) bases
    processVictory dur wnr = stgC { stageScoreScores    = newScore
                                  , stageScoreGameCount = gameCount + 1
                                  , stageScoreDuration  = dur
                                  }
      where
        newScore = case wnr of
                     Just fl | fl == t1fl -> (first (+1)) scores
                             | fl == t2fl -> (second (+1)) scores
                     _                    -> scores

        (StageScore scores gameCount _) = stgC


basesWire :: (MonadFix m, Monoid e, HasTime Double s)
  => (TeamFlag, TeamFlag)
  -> [Base]
  -> Wire s e m (([Soldier],[Soldier]),[Event [Attack]]) ([Base],([BaseEvents],[BaseEvents]))
basesWire fls@(t1fl,_t2fl) b0s = proc (inp,atks) -> do

  bEvts <- zipWires (map (baseWire fls) b0s) -< zip (repeat inp) atks

  let
    (bases,swaps) = unzip bEvts
    evts = foldr sortEvts ([],[]) swaps

  returnA -< (bases,evts)
  where
    sortEvts swaps (t1bes,t2bes) =
        case swaps of
          Event j@(Just fl) | fl == t1fl -> ( Event [GetBase]  : t1bes, Event [LoseBase j]          : t2bes )
                          | otherwise  -> ( Event [LoseBase j]          : t1bes, Event [GetBase]  : t2bes )
          Event _                      -> ( Event [LoseBase Nothing] : t1bes, Event [LoseBase Nothing] : t2bes )
          NoEvent                      -> ( NoEvent          : t1bes, NoEvent          : t2bes )

baseWire :: forall m e s. (MonadFix m, Monoid e, HasTime Double s)
  => (TeamFlag, TeamFlag)
  -> Base
  -> Wire s e m (([Soldier],[Soldier]), Event [Attack]) (Base,Event (Maybe TeamFlag))
baseWire (t1fl,t2fl) b0@(Base pb fl0 _ _ _) = proc ((t1s,t2s),atks) -> do
  let
    t1b = length $ filter inBase t1s
    t2b = length $ filter inBase t2s
    influence | t1b > t2b = Just t1fl
              | t1b < t2b = Just t2fl
              | otherwise = Nothing

  rec
    let newWire = ntBase <$> teamChange
    ((security, leaning), teamChange) <- drSwitch (ntBase fl0) -< ((influence, atks), newWire)

  owner <- hold <|> pure fl0 -< teamChange

  let newBase = b0 { baseTeamFlag = owner
                   , baseSecurity = security
                   , baseLeaning  = leaning
                   }

  returnA -< (newBase, teamChange)

  where
    inBase = (< baseRadius) . norm . (^-^ pb) . getPos
    ntBase :: Maybe TeamFlag -> Wire s e m (Maybe TeamFlag, Event [Attack]) ((Double, Maybe TeamFlag), Event (Maybe TeamFlag))
    ntBase = maybe neutralBase teamBase
    neutralBase :: Wire s e m (Maybe TeamFlag, Event [Attack]) ((Double, Maybe TeamFlag), Event (Maybe TeamFlag))
    neutralBase = proc (infl,_) -> do
      rec
        let push =
              case infl of
                Just fl | fl == t1fl -> 1
                        | otherwise  -> -1
                Nothing -> -0.25 * signum sec
        sec <- integral 0 -< push

      let leaning | sec > 0   = Just t1fl
                  | sec < 0   = Just t2fl
                  | otherwise = Nothing

      swap <- never . W.when ((< baseThreshold) . abs) --> now -< sec

      returnA -< ((1 - (abs sec / baseThreshold),leaning),leaning <$ swap)
    teamBase :: TeamFlag -> Wire s e m (Maybe TeamFlag, Event [Attack]) ((Double, Maybe TeamFlag), Event (Maybe TeamFlag))
    teamBase fl = proc (infl,atks) -> do
      rec
        let push =
              case infl of
                Just fl' | fl' == fl -> 1
                         | otherwise -> -1
                Nothing              -> 0.1
        sec <- integralWith (\_ s' -> (min s' baseThreshold)) baseThreshold -< (push,())

      swap <- never . W.when (> 0) --> now -< sec

      returnA -< ((sec / baseThreshold,Nothing),Nothing <$ swap)

