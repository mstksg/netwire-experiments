module Experiment.Battlefield.Stats where

import Experiment.Battlefield.Types

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

