module Experiment.Battlefield.Stats where

import Experiment.Battlefield.Types
import Data.Maybe (isJust)

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

weaponRange :: Weapon -> Maybe Double
weaponRange Sword   = Nothing
weaponRange Axe     = Nothing
-- weaponRange Sword   = Just 7.5
-- weaponRange Axe     = Just 7.5
weaponRange Bow     = Just 50
weaponRange Longbow = Just 100

weaponRanged :: Weapon -> Bool
weaponRanged = isJust . weaponRange

weaponCooldown :: Weapon -> Double
weaponCooldown Sword   = 0.5
weaponCooldown Axe     = 1.25
weaponCooldown Bow     = 1
weaponCooldown Longbow = 1.5

recoveryFactor :: Double
recoveryFactor = 21

adrenalineHalflife :: Double
adrenalineHalflife = 1

adrenalineDamage :: Double
adrenalineDamage = 2            -- dmg * (adr * adrAtk + 1)

adrenalineRecovery :: Double
adrenalineRecovery = 2

adrenalineSpeed :: Double
adrenalineSpeed = 0.5

unoptimalRange :: Double
unoptimalRange = 1.5

unoptimalDamage :: Double
unoptimalDamage = 1/6

rangedAccuracy :: Double
rangedAccuracy = 0.9

hitRadius :: Double
hitRadius = 5
