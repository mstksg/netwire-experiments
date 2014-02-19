module Experiment.Battlefield.Soldier (soldierWire) where

import Control.Monad
import Control.Monad.Fix
import Control.Wire                  as W
import Data.List                     (minimumBy)
import Data.Maybe                    (mapMaybe)
import Data.Ord                      (comparing)
import Experiment.Battlefield.Attack
import Experiment.Battlefield.Types
import FRP.Netwire.Move
import Linear
import Prelude hiding                ((.),id)
import Utils.Wire.Misc
import Utils.Wire.Noise

soldierWire :: (MonadFix m, HasTime t s, Monoid e, Fractional t)
    => SoldierData
    -> Wire s e m ([Soldier], SoldierInEvents) (Soldier, SoldierOutEvents)
soldierWire (SoldierData x0 fl bod weap mnt gen) =
  proc (targets,mess) -> do

    let
      targetsPos = map soldierPos targets
      attackeds = mapMaybe maybeAttacked <$> mess

    attackers <- curated -< (map snd <$> attackeds, findAttacker targetsPos)

    -- seeking and movement
    rec
      let
        -- find the target and the direction to face
        targetPool  | null attackers = targetsPos
                    | otherwise      = attackers
        target = seek targetPool pos
        newD   = target >>= newAtk pos
        dir    = snd <$> target

        -- move to target?
        vel = case target of
          Just (tDist, tDir)
            | tDist > range  -> tDir ^* speed
          _                  -> 0

      pos <- integral x0 -< vel

    -- shoot!
    shot  <- shoot -< newD
    -- this is bad frp but :|
    shotR <- couple (noisePrimR (1/damageVariance,damageVariance) gen) -< shot
    let
      shot' = (\(atk,r) -> [atk (r * baseDamage)]) <$> shotR

    -- calculate hit damage
    let
      hit = getSum . mconcat . map (Sum . fst) <$> attackeds
    damage <- hold . accumE (+) 0 <|> pure 0 -< hit

    rec
      -- calculate health, plus recovery
      let
        health = min (startingHealth + recov - damage) startingHealth
      recov <- integral 0 . ((pure recovery . W.when (< startingHealth)) <|> pure 0) . delay startingHealth -< health

    -- calculate last direction facing.  holdJust breaks FRP.
    (V3 vx vy _) <- holdJust zero -< dir

    let
      angle = atan2 vy vx
      soldier = Soldier (PosAng pos angle) (health / startingHealth) fl bod weap mnt

    -- inhibit when health > 0
    W.unless (<= 0) -< health
    returnA -< (soldier,shot')

  where
    newAtk p (tDist, tDir)
      | tDist > range = Nothing
      | otherwise     = Just $ AttackEvent . AttackData aX0 tDir . flip (Attack weap) aX0
          where
            aX0 = p ^+^ (tDir ^* 7.5)
    range = weaponRange weap
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
    -- findAttacker others pos = snd <$> mfilter ((< 9) . fst . fst) (findClosest others pos)
    findAttacker others pos = snd <$> mfilter ((< 9) . fst . fst) (findClosest others pos)
    -- seekOrigin :: V3 Double -> Event [V3 Double] -> V3 Double
    -- seekOrigin pos attackedFrom
    --   | null <$> attackedFrom = pos
    seek others = (fst <$>) . findClosest others
    shoot = (proc newA -> do
      case newA of
        Nothing -> never -< ()
        Just a  -> do
          shot <- W.for coolDownTime . now -< a
          returnA -< shot
      ) --> shoot
    -- angle = proc (V3 vx vy _) -> do

bodyHealth :: SoldierBody -> Double
bodyHealth MeleeBody  = 25
bodyHealth TankBody   = 55
bodyHealth RangedBody = 12

mountHealthMod :: Mount -> Double
mountHealthMod Foot  = 1
mountHealthMod Horse = 1.5

mountSpeed :: Mount -> Double
mountSpeed Foot   = 25
mountSpeed Horse  = 75

bodySpeedMod :: SoldierBody -> Double
bodySpeedMod MeleeBody  = 1
bodySpeedMod TankBody   = 0.5
bodySpeedMod RangedBody = 1

weaponDPS :: Weapon -> Double
weaponDPS Sword   = 8.0
weaponDPS Axe     = 10.0
weaponDPS Bow     = 2.75
weaponDPS Longbow = 2.5

mountDamageMod :: Mount -> Double
mountDamageMod Foot  = 1
mountDamageMod Horse = 1.25

damageVariance :: Double
damageVariance = 1.15

weaponRange :: Weapon -> Double
weaponRange Sword   = 7.5
weaponRange Axe     = 7.5
weaponRange Bow     = 50
weaponRange Longbow = 100

weaponCooldown :: Fractional a => Weapon -> a
weaponCooldown Sword   = 0.5
weaponCooldown Axe     = 1.25
weaponCooldown Bow     = 1
weaponCooldown Longbow = 1.5

recoveryFactor :: Double
recoveryFactor = 21
