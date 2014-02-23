{-# LANGUAGE TupleSections #-}

module Experiment.Battlefield.Stage (stageWire) where

import Control.Monad.Fix
import Control.Wire                 as W
import Control.Wire.Unsafe.Event
import Data.Maybe                   (catMaybes)
import Data.Traversable
import Experiment.Battlefield.Team
import Experiment.Battlefield.Types
import FRP.Netwire.Move
import Linear.Metric
import Linear.V3
import Linear.Vector
import Prelude hiding               ((.),id)

stageWire :: (MonadFix m, Monoid e, HasTime Double s)
  => (Double, Double)
  -> TeamData
  -> TeamData
  -> Wire s e m () Stage
stageWire dim@(w,h) t1d t2d = proc _ -> do
    let t1bes' = repeat NoEvent
        t2bes' = repeat NoEvent

    rec
      (team1@(Team _ t1ss t1as _t1bs), t2ahits) <- t1w . delay teamWireDelayer -< (team2, (t1bes', t1ahits))
      (team2@(Team _ t2ss t2as _t2bs), t1ahits) <- t2w . delay teamWireDelayer -< (team1, (t2bes', t2ahits))

      let t1ss' = catMaybes t1ss
          t2ss' = catMaybes t2ss

      (bases,(t1bes,t2bes)) <- basesWire (t1fl,t2fl) b0s . delay ([],[]) -< (t1ss',t2ss')

    let sldrs = t1ss' ++ t2ss'
        arts  = t1as ++ t2as

    returnA -< Stage dim sldrs arts bases

  where
    t1fl = teamDataFlag t1d
    t2fl = teamDataFlag t2d

    t1w = teamWire b0s t1d
    t2w = teamWire b0s t2d
    b1s = makeBase t1fl <$> [V3 (w/6) (h/6) 0, V3 (w/6) (5*h/6) 0]
    b2s = makeBase t2fl <$> [V3 (5*w/6) (h/6) 0, V3 (5*w/6) (5*h/6) 0]
    b0s = b1s ++ b2s
    makeBase fl pb = Base pb (Just fl) 1 Nothing

basesWire :: (MonadFix m, Monoid e, HasTime Double s)
  => (TeamFlag, TeamFlag)
  -> [Base]
  -> Wire s e m ([Soldier],[Soldier]) ([Base],([BaseEvents],[BaseEvents]))
basesWire fls@(t1fl,_t2fl) b0s = proc inp -> do

  bEvts <- sequenceA (map (baseWire fls) b0s) -< inp

  let
    (bases,swaps) = unzip bEvts
    evts = foldl sortEvts ([],[]) (reverse swaps)

  returnA -< (bases,evts)
  where
    sortEvts (t1bes,t2bes) swaps =
        case swaps of
          Event (Just fl,_) | fl == t1fl -> (Event [GetBase]:t1bes,NoEvent:t2bes)
                            | otherwise  -> (NoEvent:t1bes,Event [GetBase]:t2bes)
          Event (_,Just fl) | fl == t1fl -> (Event [LoseBase]:t1bes,NoEvent:t2bes)
                            | otherwise  -> (NoEvent:t1bes,Event [LoseBase]:t2bes)
          NoEvent                        -> (NoEvent:t1bes,NoEvent:t2bes)

baseWire :: forall m e s. (MonadFix m, Monoid e, HasTime Double s)
  => (TeamFlag, TeamFlag)
  -> Base
  -> Wire s e m ([Soldier],[Soldier]) (Base,Event (Maybe TeamFlag, Maybe TeamFlag))
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
  oldOwner <- delay fl0 -< owner

  let newBase = b0 { baseTeamFlag = owner
                   , baseSecurity = security
                   , baseLeaning  = leaning
                   }

      changeEvent = (,oldOwner) <$> teamChange

  returnA -< (newBase, changeEvent)

  where
    thresh = 5
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
                Nothing -> -0.25 * sec / abs sec
        sec <- integral 0 -< push

      let leaning | sec > 0   = Just t1fl
                  | sec < 0   = Just t2fl
                  | otherwise = Nothing

      swap <- never . W.when ((< thresh) . abs) --> now -< sec

      returnA -< ((1 - (abs sec / thresh),leaning),infl <$ swap)
    teamBase :: TeamFlag -> Wire s e m (Maybe TeamFlag) ((Double, Maybe TeamFlag), Event (Maybe TeamFlag))
    teamBase fl = proc infl -> do
      rec
        let push =
              case infl of
                Just fl' | fl' == fl -> 1
                         | otherwise -> -1
                Nothing              -> 0.1
        sec <- integralWith (\_ s' -> (min s' thresh)) thresh -< (push,())

      swap <- never . W.when (> 0) --> now -< sec

      returnA -< ((sec / thresh,Nothing),Nothing <$ swap)

