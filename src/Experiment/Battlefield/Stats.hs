module Experiment.Battlefield.Stats where

import Experiment.Battlefield.Types
import Data.Maybe (isJust)

bodyHealth :: SoldierBody -> Double
bodyHealth MeleeBody  = 25
bodyHealth TankBody   = 60
bodyHealth RangedBody = 15

mountHealthMod :: Mount -> Double
mountHealthMod Foot  = 1
mountHealthMod Horse = 1.5

mountSpeed :: Mount -> Double
mountSpeed Foot   = 25
mountSpeed Horse  = 75

bodySpeedMod :: SoldierBody -> Double
bodySpeedMod MeleeBody  = 1
bodySpeedMod TankBody   = 0.4
bodySpeedMod RangedBody = 1.2

weaponDPS :: Weapon -> Double
weaponDPS Sword   = 7.5
weaponDPS Axe     = 12.5
weaponDPS Bow     = 2.75
weaponDPS Longbow = 4

mountDamageMod :: Mount -> Double
mountDamageMod Foot  = 1
mountDamageMod Horse = 1.25

damageVariance :: Double
damageVariance = 1.15

weaponRange :: Weapon -> Maybe Double
weaponRange Sword   = Nothing
weaponRange Axe     = Nothing
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

maximumRange :: Double
maximumRange = 2.0

rangedAccuracy :: Double
rangedAccuracy = 0.85

baseThreshold :: Double
baseThreshold = 10

