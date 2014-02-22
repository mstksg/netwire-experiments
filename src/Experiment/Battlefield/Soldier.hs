module Experiment.Battlefield.Soldier
  ( soldierWire
  , swordsmanClass
  , archerClass
  , axemanClass
  , longbowmanClass
  , horsemanClass
  , horsearcherClass
  ) where

import Control.Monad
import Control.Monad.Fix
import Control.Wire                  as W
import Control.Wire.Unsafe.Event
import Data.List                     (minimumBy, mapAccumL)
import Data.Maybe                    (mapMaybe, catMaybes, fromMaybe)
import Data.Ord                      (comparing)
import Experiment.Battlefield.Attack
import Experiment.Battlefield.Stats
import Experiment.Battlefield.Types
import FRP.Netwire.Move
-- import FRP.Netwire.Noise
import Linear
import Prelude hiding                ((.),id)
import System.Random
import Utils.Helpers                 (foldAcrossl)
import Utils.Wire.Misc
import Utils.Wire.Noise
import Utils.Wire.Wrapped

soldierWire :: (MonadFix m, HasTime Double s, Monoid e)
    => SoldierData
    -> Wire s e m ([Maybe Soldier], SoldierInEvents) ((Maybe Soldier,[Article]), (SoldierOutEvents,[SoldierInEvents]))
soldierWire (SoldierData x0 fl (SoldierClass bod weap mnt) gen) =
  proc (targets,mess) -> do

    -- it's good to be alive!
    age <- integral 0 -< 1

    let
      targetsPos = map soldierPos (catMaybes targets)
      attackeds = mapMaybe maybeAttacked <$> mess

    attackers <- curated -< (map snd <$> attackeds, findAttacker targetsPos)

    -- calculate damage from hits
    let
      hit = sum . map fst <$> attackeds
    damage <- hold . accumE (+) 0 <|> pure 0 -< hit

    rec
      -- calculate health, plus recovery
      let
        health = min (startingHealth + recov - damage) startingHealth
        alive = health > 0
      recov <- integral 0 . ((pure recovery . W.when (< startingHealth)) <|> pure 0) . delay startingHealth -< health

    -- seeking and movement
    acc <- accuracy -< ()
    rec
      let
        -- find the target and the direction to face
        targetPool  | null attackers || not isRanged = targetsPos
                    | otherwise                      = attackers
        target = seek targetPool pos
        newD
          | alive     = target >>= newAtk pos acc
          | otherwise = Nothing
        dir    = snd <$> target

        -- move to target?
        vel = case target of
          Just (tDist, tDir)
            | tDist > range  -> tDir ^* speed
          _                  -> 0

      pos <- integral x0 -< vel

    -- calculate last direction facing.  holdJust breaks FRP.
    (V3 vx vy _) <- holdJust zero -< dir

    -- shoot!
    shot  <- shoot -< newD
    let
      shotW = map attackWire <$> shot

    -- manage shots
    rec
      atks    <- dWireBox' NoEvent -< (shotW, atkDies)
      let
        atkHitsKills = checkAttacks atks targets
        (kills,atkHits) = unzip atkHitsKills
        kills' = (length . filter id) kills
        killE | kills' > 0 = Event kills'
              | otherwise  = NoEvent
        atkDies = map (mconcat . map (() <$)) atkHits
        atkOuts = foldAcrossl (<>) mempty atkHits

    killCount <- hold . accumE (+) 0 <|> 0 -< killE

    let
      angle = atan2 vy vx
      hasAtks = not (null atks)

      wouldKill = (>= health) . attackDamage
      funcs     = SoldierFuncs wouldKill
      stats     = SoldierStats killCount age

      soldier       = Soldier
                        (PosAng pos angle)
                        (health / startingHealth)
                        fl stats funcs bod weap mnt
      soldier'
        | alive     = Just soldier
        | otherwise = Nothing

    -- inhibit when dead and no more attacks
    W.when (uncurry (||)) -< (alive, hasAtks)

    outE <- never -< ()
    returnA -< ((soldier',atks),(outE,atkOuts))

  where
    (dmgGen,accGen') = split gen
    (_firstAcc,_accGen) = randomR (-accLimit,accLimit) accGen'
    accLimit = atan $ (hitRadius / rangedAccuracy / 2) / range
    isRanged = weaponRanged weap
    accuracy
      -- | isRanged  = Just <$> (hold . noiseR coolDownTime (-accLimit,accLimit) accGen <|> pure firstAcc)
      | otherwise = pure Nothing
    newAtk :: V3 Double -> Maybe Double -> (Double, V3 Double) -> Maybe (Double -> AttackData)
    newAtk p acc (tDist, tDir)
      | tDist > range = Nothing
      | otherwise     = Just $ AttackData aX0 tDir' . flip (Attack weap) aX0
          where
            tDir' =
              case acc of
                Just a  -> (rot2 a) !* tDir
                Nothing -> tDir
            aX0 = p ^+^ (tDir' ^* 7.5)
    rot2 ang = V3 (V3 (cos ang) (-1 * (sin ang)) 0)
                  (V3 (sin ang) (cos ang)        0)
                  (V3 0         0                1)
    range = fromMaybe 7.5 $ weaponRange weap
    startingHealth = bodyHealth bod * mountHealthMod mnt
    speed = mountSpeed mnt * bodySpeedMod bod
    recovery = startingHealth / recoveryFactor
    baseDamage = weaponDPS weap * (realToFrac coolDownTime) * mountDamageMod mnt
    coolDownTime = weaponCooldown weap
    findClosest :: [V3 Double] -> V3 Double -> Maybe ((Double, V3 Double),V3 Double)
    findClosest [] _       = Nothing
    findClosest others pos = Just $ minimumBy (comparing fst) otherPs
      where
        otherPs = map f others
        f v = ((d,u),v)
          where
            dv = v ^-^ pos
            d  = norm dv
            u  = dv ^/ d
    findAttacker :: [V3 Double]  -> V3 Double -> Maybe (V3 Double)
    findAttacker others pos = snd <$> mfilter ((< 9) . fst . fst) (findClosest others pos)
    seek others = (fst <$>) . findClosest others
    shoot = fmap applyRandom <$> coupleRandom . oneShot
      where
        oneShot = (proc newA -> do
          case newA of
            Nothing -> never -< ()
            Just a  -> do
              shot <- W.for coolDownTime . now -< a
              returnA -< shot
          ) --> oneShot
        coupleRandom = couple (noisePrimR (1/damageVariance,damageVariance) dmgGen)
        applyRandom (atk,r) = [atk (r * baseDamage)]
    checkAttacks :: [Article] -> [Maybe Soldier] -> [(Bool,[SoldierInEvents])]
    checkAttacks atks sldrs = map makeKill atks
      where
        makeKill :: Article -> (Bool, [SoldierInEvents])
        makeKill (Article (PosAng pa _)
                 (ArticleAttack atk@(Attack _ dmg o)))
                    = first (fromMaybe False) $ mapAccumL f Nothing sldrs
          where
            f b      Nothing         = (b    , NoEvent)
            f b@(Just _)   _               = (b, NoEvent)
            f Nothing (Just sldr)
              | norm (pa ^-^ ps) < hitRadius = (Just killed, Event [AttackedEvent dmg o])
              | otherwise                    = (Nothing, NoEvent )
                  where
                    ps     = soldierPos sldr
                    killed = soldierFuncsWouldKill (soldierFuncs sldr) atk

swordsmanClass   :: SoldierClass
archerClass      :: SoldierClass
axemanClass      :: SoldierClass
longbowmanClass  :: SoldierClass
horsemanClass    :: SoldierClass
horsearcherClass :: SoldierClass

swordsmanClass   = SoldierClass MeleeBody Sword Foot
archerClass      = SoldierClass RangedBody Bow Foot
axemanClass      = SoldierClass TankBody Axe Foot
longbowmanClass  = SoldierClass RangedBody Longbow Foot
horsemanClass    = SoldierClass MeleeBody Sword Horse
horsearcherClass = SoldierClass RangedBody Bow Horse
