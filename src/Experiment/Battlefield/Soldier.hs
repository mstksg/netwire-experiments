module Experiment.Battlefield.Soldier (soldierWire) where

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

    -- seeking and movement
    rec
      let
        -- find the target and the direction to face
        target = seek targets pos
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
    shotR <- couple (noisePrimR (0.75 :: Double,1.5) gen) -< shot
    let
      shot' = (\(atk,r) -> [atk (r * baseDamage)]) <$> shotR

    -- calculate hit damage
    let
      hit = getSum . mconcat . fmap Sum . mapMaybe maybeAttacked <$> mess
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
      | otherwise     = Just $ AttackEvent . AttackData (p ^+^ (tDir ^* 8)) tDir . Attack weap
    range = weaponRange weap
    startingHealth = bodyHealth bod * mountHealthMod mnt
    speed = mountSpeed mnt * bodySpeedMod bod
    recovery = 0.15
    baseDamage = weaponDamage weap * mountDamageMod mnt
    coolDownTime = weaponCooldown weap
    seek others pos | null otherPs = Nothing
                    | otherwise    = Just $ minimumBy (comparing fst) otherPs
      where
        otherPs = map ( (fst &&& uncurry (flip (^/)))
                      . (norm &&& id)
                      . (^-^ pos)
                      . posAngPos
                      . soldierPosAng
                      ) others
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
bodyHealth TankBody   = 60
bodyHealth RangedBody = 12

mountHealthMod :: Mount -> Double
mountHealthMod Foot  = 1
mountHealthMod Horse = 1.5

mountSpeed :: Mount -> Double
mountSpeed Foot   = 5
mountSpeed Horse  = 15

bodySpeedMod :: SoldierBody -> Double
bodySpeedMod MeleeBody  = 1
bodySpeedMod TankBody   = 0.5
bodySpeedMod RangedBody = 1

weaponDamage :: Weapon -> Double
weaponDamage Sword   = 4
weaponDamage Axe     = 8
weaponDamage Bow     = 2.5
weaponDamage Longbow = 2.5

mountDamageMod :: Mount -> Double
mountDamageMod Foot  = 1
mountDamageMod Horse = 1.25

weaponRange :: Weapon -> Double
weaponRange Sword   = 7.5
weaponRange Axe     = 7.5
weaponRange Bow     = 50
weaponRange Longbow = 100

weaponCooldown :: Fractional a => Weapon -> a
weaponCooldown Sword   = 3
weaponCooldown Axe     = 6
weaponCooldown Bow     = 3.5
weaponCooldown Longbow = 4.5
