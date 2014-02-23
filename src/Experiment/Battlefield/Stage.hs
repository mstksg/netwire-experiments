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
    b10 <- now -< b1s
    b20 <- now -< b2s

    let
      b1es = repeat NoEvent
      b2es = repeat NoEvent

    rec
      (team1@(Team _ t1ss t1as t1bs), t2ahits) <- t1w . delay teamWireDelayer -< (team2, ((b10,b1es), t1ahits))
      (team2@(Team _ t2ss t2as t2bs), t1ahits) <- t2w . delay teamWireDelayer -< (team1, ((b20,b2es), t2ahits))

    let sldrs = catMaybes (t1ss ++ t2ss)
        arts  = t1as ++ t2as
        bases = t1bs ++ t2bs

    returnA -< Stage dim sldrs arts bases

  where
    t1w = teamWire t1d
    t2w = teamWire t2d
    b1s = GotBase . makeBase <$> [V3 (w/6) (h/6) 0, V3 (w/6) (5*h/6) 0]
    b2s = GotBase . makeBase <$> [V3 (5*w/6) (h/6) 0, V3 (5*w/6) (5*h/6) 0]
    makeBase pb = Base pb Nothing 1 Nothing

basesWire :: (MonadFix m, Monoid e, HasTime Double s)
  => (TeamFlag, TeamFlag)
  -> [Base]
  -> Wire s e m ([Soldier],[Soldier]) ([Base],((TeamInEvents,[BaseEvents]),(TeamInEvents,[BaseEvents])))
basesWire fls@(t1fl,t2fl) b0s = proc inp -> do

  bEvts <- sequenceA (map (baseWire fls) b0s) -< inp

  let
    bases = map fst bEvts
    evts = foldl sortEvts ((NoEvent,[]),(NoEvent,[])) (reverse bEvts)

  returnA -< (bases,evts)
  where
    sortEvts ((t1es,t1bes),(t2es,t2bes)) (base,swaps) =
      -- ((t1es',t1bes'),(t2es',t2bes'))
        case swaps of
          Event (Just fl,_) | fl == t1fl -> ((t1es <> ([GotBase base] <$ swaps),NoEvent:t1bes),(t2es,NoEvent:t2bes))
                            | otherwise  -> ((t1es,NoEvent:t1bes),(t2es <> ([GotBase base] <$ swaps),NoEvent:t2bes))
          Event (_,Just fl) | fl == t1fl -> ((t1es,Event [LoseBase]:t1bes),(t2es,NoEvent:t2bes))
                            | otherwise  -> ((t1es,NoEvent:t1bes),(t2es,Event [LoseBase]:t2bes))
          NoEvent ->    ((t1es,NoEvent:t1bes),(t2es,NoEvent:t2bes))



      where
        bfl = baseTeamFlag base

      --   (t1bes',) =
      --   t1es' = t1es
      --   t1bes' =
      --     case swaps of
      --       NoEvent -> NoEvent:t1bes
      --       Event (Just fl) | fl ==
      --   t2es' = t2es
      --   t2bes' = t2bes

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
    ((security, leaning), teamChange) <- rSwitch (ntBase fl0) -< (influence, newWire)

  owner <- hold <|> pure fl0 -< teamChange
  oldOwner <- delay fl0 -< owner

  let newBase = b0 { baseTeamFlag = owner
                   , baseSecurity = security
                   , baseLeaning  = leaning
                   }

      changeEvent = (,oldOwner) <$> teamChange

  -- let mode = 

  --       case owner of
  --         Nothing              -> undefined
  --         Just fl | fl == t1fl -> undefined
  --                 | otherwise  -> undefined

  -- mode -< influence

  returnA -< (newBase, changeEvent)

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
                Nothing -> -1 * sec / abs sec
        sec <- integral 0 -< push

      let leaning | sec > 0   = Just t1fl
                  | sec < 0   = Just t2fl
                  | otherwise = Nothing

      swap <- never . W.when ((< 1) . abs) --> now -< sec
      returnA -< ((sec,leaning),infl <$ swap)
    teamBase :: TeamFlag -> Wire s e m (Maybe TeamFlag) ((Double, Maybe TeamFlag), Event (Maybe TeamFlag))
    teamBase fl = proc infl -> do
      rec
        let push =
              case infl of
                Just fl' | fl' == fl -> 1
                         | otherwise -> -1 * sec
        sec <- integral 0 -< push

      swap <- never . W.when (< 1) --> now -< sec

      returnA -< ((sec,Nothing),Nothing <$ swap)

    -- mode = modes (Just fl0)

