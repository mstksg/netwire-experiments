{-# LANGUAGE TupleSections #-}

module Experiment.Battlefield.Stage (stageWire) where

import Control.Monad.Fix
import Control.Wire                 as W
import Data.Default
import Control.Wire.Unsafe.Event
import Data.Maybe                   (catMaybes)
import Data.Traversable
import Experiment.Battlefield.Stats
import Experiment.Battlefield.Team
import Experiment.Battlefield.Types
import FRP.Netwire.Move
import Linear.Metric
import Linear.V3
import Linear.Vector
import Prelude hiding               ((.),id)

stageWire :: (MonadFix m, Monoid e, HasTime Double s)
  => StageData
  -> (Double, Double)
  -> TeamData
  -> TeamData
  -> Wire s e m () Stage
stageWire stgD dim@(w,h) t1d t2d = proc _ -> do

    rec
      (team1@(Team _ t1ss t1as _), t2ahits) <- t1w . delay (teamWireDelayer b0s) -< ((team2,bases), (t1bes, t1ahits))
      (team2@(Team _ t2ss t2as _), t1ahits) <- t2w -< ((team1,bases), (t2bes, t2ahits))

      let t1ss' = catMaybes t1ss
          t2ss' = catMaybes t2ss

      (bases,(t1bes,t2bes)) <- basesWire (t1fl,t2fl) b0s . delay ([],[]) -< (t1ss',t2ss')

    let sldrs = t1ss' ++ t2ss'
        arts  = t1as  ++ t2as

    returnA -< Stage dim def sldrs arts bases

  where
    t1fl = teamDataFlag t1d
    t2fl = teamDataFlag t2d

    t1w = teamWire b0s t1d
    t2w = teamWire b0s t2d
    -- b1s = makeBase t1fl <$> [V3 (w/6) (h/6) 0, V3 (w/6) (5*h/6) 0]
    -- b2s = makeBase t2fl <$> [V3 (5*w/6) (h/6) 0, V3 (5*w/6) (5*h/6) 0]
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
    bns = makeBase Nothing <$> [V3 (w/2) (h/4) 0, V3 (w/2) (3*h/4) 0, V3 (w/4) (h/2) 0, V3 (3*w/4) (h/2) 0, V3 (5*w/6) (h/6) 0, V3 (w/2) (h/2) 0, V3 (w/6) (5*h/6) 0]
    b0s = b1s ++ bns ++ b2s
    makeBase fl pb = Base pb fl 1 Nothing

basesWire :: (MonadFix m, Monoid e, HasTime Double s)
  => (TeamFlag, TeamFlag)
  -> [Base]
  -> Wire s e m ([Soldier],[Soldier]) ([Base],([BaseEvents],[BaseEvents]))
basesWire fls@(t1fl,_t2fl) b0s = proc inp -> do

  bEvts <- sequenceA (map (baseWire fls) b0s) -< inp

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
  -> Wire s e m ([Soldier],[Soldier]) (Base,Event (Maybe TeamFlag))
baseWire (t1fl,t2fl) b0@(Base pb fl0 _ _) = proc (t1s,t2s) -> do
  let
    t1b = length $ filter inBase t1s
    t2b = length $ filter inBase t2s
    influence | t1b > t2b = Just t1fl
              | t1b < t2b = Just t2fl
              | otherwise = Nothing

  rec
    let newWire = ntBase <$> teamChange
    ((security, leaning), teamChange) <- drSwitch (ntBase fl0) -< (influence, newWire)

  owner <- hold <|> pure fl0 -< teamChange

  let newBase = b0 { baseTeamFlag = owner
                   , baseSecurity = security
                   , baseLeaning  = leaning
                   }

  returnA -< (newBase, teamChange)

  where
    inBase = (< baseRadius) . norm . (^-^ pb) . soldierPos
    ntBase :: Maybe TeamFlag -> Wire s e m (Maybe TeamFlag) ((Double, Maybe TeamFlag), Event (Maybe TeamFlag))
    ntBase = maybe neutralBase teamBase
    neutralBase :: Wire s e m (Maybe TeamFlag) ((Double, Maybe TeamFlag), Event (Maybe TeamFlag))
    neutralBase = proc infl -> do
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
    teamBase :: TeamFlag -> Wire s e m (Maybe TeamFlag) ((Double, Maybe TeamFlag), Event (Maybe TeamFlag))
    teamBase fl = proc infl -> do
      rec
        let push =
              case infl of
                Just fl' | fl' == fl -> 1
                         | otherwise -> -1
                Nothing              -> 0.1
        sec <- integralWith (\_ s' -> (min s' baseThreshold)) baseThreshold -< (push,())

      swap <- never . W.when (> 0) --> now -< sec

      returnA -< ((sec / baseThreshold,Nothing),Nothing <$ swap)

