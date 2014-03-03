{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}

module Experiment.Battlefield.Stage
  ( stageWireOnce
  , stageWireLoop
  , stageWireOnce'
  , stageWireLoop'
  ) where

import Data.Traversable (mapAccumL)
-- import Data.Foldable (foldl)
import Debug.Trace
import Control.Monad.Fix
import Control.Wire                 as W
import Utils.Wire.Debug
import Control.Wire.Unsafe.Event
import Data.Default
import Data.Maybe                   (isNothing, catMaybes)
import Experiment.Battlefield.Stats
import Experiment.Battlefield.Team
import Experiment.Battlefield.Types
import FRP.Netwire.Move
import Experiment.Battlefield.Attack
import Linear.Metric
import Linear.V3
import Linear.Vector
import Prelude hiding               ((.),id,foldl)
import System.Random
import Utils.Helpers
import Utils.Wire.Misc
import Utils.Wire.Wrapped
import qualified Data.Map.Strict    as M
import Data.Map.Strict (Map)

type ArticleID = UUID
type SoldierID = UUID

deriving instance Show a => Show (Event a)

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
      -- manage teams
      (team1@(Team _ t1ss _), t1Outs) <- t1w . delay (teamWireDelayer b0s) -< ((team2,bases), (t1bes, t1InAll))
      (team2@(Team _ t2ss _), t2Outs) <- t2w . delay (teamWireDelayer b0s) -< ((team1,bases), (t2bes, t2InAll))

      let t1ss' = M.elems t1ss
          t2ss' = M.elems t2ss
          t1asn = Event $ concatMap makeAttackWire (M.toList t1Outs)
          t2asn = Event $ concatMap makeAttackWire (M.toList t2Outs)

      -- manage arrows
      t1as <- dWireMap NoEvent uuids -< (t1asn,t1aIns)
      t2as <- dWireMap NoEvent uuids -< (t2asn,t2aIns)

      let (t1as',t2bhits)         = findWallHits t1as (Just t1fl) bases
          (t2as',t1bhits)         = findWallHits t2as (Just t2fl) bases
          ((t1Ins,t1aIns),t2hIns) = findHits t1as' t2ss
          ((t2Ins,t2aIns),t1hIns) = findHits t2as' t1ss
          arts = map snd (M.elems t1as ++ M.elems t2as)
          t1InAll = M.unionWith (<>) t1Ins t1hIns
          t2InAll = M.unionWith (<>) t2Ins t2hIns

      (bases,(t1bes,t2bes)) <- basesWire (t1fl,t2fl) b0s . delay (([],[]),repeat NoEvent) -< ((t1ss',t2ss'),zipWith (<>) t2bhits t1bhits)

    let sldrs = t1ss' ++ t2ss'

    victory <- never . W.when isNothing --> now -< winner bases

    let newData = processVictory duration <$> victory

    returnA -< (Stage dim (stgC { stageScoreDuration = duration }) sldrs arts bases, newData)

  where
    makeAttackWire (sId,Event es) = map (\d -> (sId,) <$> attackWire d) atkDatas
      where
        atkDatas = [ atkData | AttackEvent atkData <- es ]
    makeAttackWire _ = []
    findWallHits :: Map UUID (UUID, Article) -> Maybe TeamFlag -> [Base] -> (Map UUID (UUID, Article),[Event [Attack]])
    -- findWallHits as fl bs = if M.size bAtks > 0 then (M.elemAt 0 bAtks `traceShow` (leftoverArts, evts)) else (leftoverArts, evts)
    findWallHits as fl bs = (leftoverArts, evts)
      where
        atks = fmap makeHit as
        leftoverArts = fst <$> M.filter (not . fst . snd) atks
        evts = foldAcrossl (<>) NoEvent (snd . snd <$> atks)
        makeHit ua@(_, art) = (ua, mapAccumL f False bs)
          where
            f :: Bool -> Base -> (Bool, Event [Attack])
            f True _ = (True, NoEvent)
            f False b
              | baseTeamFlag b == fl && hittableHit b art = "hey" `trace` (True, Event [atk])
              | otherwise                                 = (False, NoEvent)
              where
                ArticleAttack atk = articleType art
    findHits :: Map UUID (UUID, Article) -> Map UUID Soldier -> ((Map UUID SoldierInEvents,Map UUID (Event ())), Map UUID SoldierInEvents)
    findHits as ss = ((sldrKillsEs, artHitEs), sldrHitEs)
      where
        (sldrKillsElms,sldrHitElms) = unzip (M.elems atks)
        sldrHitEs :: Map UUID SoldierInEvents
        sldrHitEs = M.unionsWith (<>) sldrHitElms
        sldrKillsEs :: Map UUID SoldierInEvents
        sldrKillsEs = M.fromList (catMaybes sldrKillsElms)
        artHitEs :: M.Map UUID (Event ())
        artHitEs = fmap ((() <$) . maybe NoEvent Event . fst) atks
        atks :: Map ArticleID (Maybe (SoldierID,SoldierInEvents), Map SoldierID SoldierInEvents)
        atks = fmap makeKill as
        makeKill :: (SoldierID, Article) -> (Maybe (SoldierID, SoldierInEvents), Map SoldierID SoldierInEvents)
        makeKill (sId,art) = M.mapAccum f Nothing ss
          where
            f :: Maybe (SoldierID, SoldierInEvents) -> Soldier -> (Maybe (SoldierID, SoldierInEvents), SoldierInEvents)
            f ht@(Just _) _                  = (ht, NoEvent)
            f Nothing     sldr
              | norm (pa ^-^ ps) < hitRadius = (Just (sId, killedEvt), Event [AttackedEvent dmg o])
              | otherwise                    = (Nothing, NoEvent)
                  where
                    ps = getPos sldr
                    pa = getPos art
                    killedEvt
                      | soldierFuncsWouldKill (soldierFuncs sldr) atk = Event [GotKillEvent sldr]
                      | otherwise = NoEvent
                    ArticleAttack atk@(Attack _ dmg o) = articleType art

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
    (((security, leaning),wall), teamChange) <- drSwitch (ntBase fl0) -< ((influence, atks), newWire)

  owner <- hold <|> pure fl0 -< teamChange

  let newBase = b0 { baseTeamFlag = owner
                   , baseSecurity = security
                   , baseLeaning  = leaning
                   , baseWall = wall
                   }

  returnA -< (newBase, teamChange)

  where
    inBase = (< baseRadius) . norm . (^-^ pb) . getPos
    ntBase :: Maybe TeamFlag -> Wire s e m (Maybe TeamFlag, Event [Attack]) (((Double, Maybe TeamFlag), Maybe Double), Event (Maybe TeamFlag))
    ntBase = maybe neutralBase teamBase
    neutralBase :: Wire s e m (Maybe TeamFlag, Event [Attack]) (((Double, Maybe TeamFlag), Maybe Double), Event (Maybe TeamFlag))
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

      returnA -< (((1 - (abs sec / baseThreshold),leaning), Nothing),leaning <$ swap)
    wallMax = 1
    wallRate = 1
    teamBase :: TeamFlag -> Wire s e m (Maybe TeamFlag, Event [Attack]) (((Double, Maybe TeamFlag), Maybe Double), Event (Maybe TeamFlag))
    teamBase fl = proc (infl,atks) -> do
      rec
        let push =
              case infl of
                Just fl' | fl' == fl -> 1
                         | otherwise -> -1
                Nothing              -> 0.1
        sec <- integralWith (\_ s' -> (min s' baseThreshold)) baseThreshold -< (push,())

      swap <- never . W.when (> 0) --> now -< sec

      wallDamage   <- (hold . accumE (+) 0) <|> pure 0 -< sum . map attackDamage <$> atks

      rec
        let wallHealth = max (wallGrowth - wallDamage) (-5 * wallRate)
        wallGrowth <- integral (-1 * wallRate) . (pure wallRate . W.when (< wallMax) <|> pure 0) . delay 0 -< wallHealth

      let wallStrength | wallGrowth < 0 = Nothing
                       | otherwise      = Just (wallGrowth / wallMax)
      -- recov <- integralWith (\d a -> min d a) 0 -< (recovery, damage)
      -- let health = maxHealth + recov - damage
      --     alive = health > 0

      -- wallDamage   <- (hold . accumE (+) 0) <|> pure 0 -< sum . map attackDamage <$> atks
      -- wallBuilding <- pure 0 . holdFor 5 <|> pure 1 -< atks
      -- wallBuilt <- pure 0 . W.for 5 --> integralWith (\_ b -> max (min b 100) 0 ) 0 -< (wallBuilding, ())
      -- let wallStrength = Just (wallBuilt - wallDamage)


      returnA -< atks `traceEvent'` (((sec / baseThreshold,Nothing), wallStrength),Nothing <$ swap)

