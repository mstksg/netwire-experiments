module Experiment.Battlefield.Attack (maybeAttacked, maybeAttack, attackWire) where

import Control.Wire                 as W
import Data.Maybe                   (fromMaybe)
import Experiment.Battlefield.Types
import Experiment.Battlefield.Stats
import FRP.Netwire.Move
import Linear.V3
import Linear.Vector
import Prelude hiding               ((.),id)

weaponSpeed :: Weapon -> Maybe Double
weaponSpeed Sword   = Nothing
weaponSpeed Axe     = Nothing
weaponSpeed Bow     = Just 400
weaponSpeed Longbow = Just 400

weaponDuration :: Fractional a => Weapon -> Maybe a
weaponDuration Sword    = Just 0.2
weaponDuration Axe      = Just 0.3
weaponDuration Bow      = Nothing
weaponDuration Longbow  = Nothing

attackWire :: (Monad m, Monoid e, HasTime t s, Fractional t)
    => AttackData
    -> Wire s e m (Event ()) Article
attackWire (AttackData x0 vu@(V3 vx vy _) atk) =
  proc die -> do
    pos      <- integral x0 -< vel
    traveled <- integral 0  -< speed
    duration                -< ()
    W.when (< unoptimal)    -< traveled
    let
      attack  | traveled < range' = optimalAtk
              | otherwise         = unoptimalAtk
    W.until                 -< (Article (PosAng pos angle) attack, die)
  where
    weap      = attackWeapon atk
    angle     = atan2 vy vx
    speed     = fromMaybe 0 (weaponSpeed weap)
    range     = fromMaybe 0 $ weaponRange weap
    range'    = range + 2.5
    unoptimal = range * unoptimalRange
    vel       = vu ^* speed
    duration  = maybe returnA W.for (weaponDuration weap)
    optimalAtk   = ArticleAttack $ atk
    unoptimalAtk = ArticleAttack $ atk { attackDamage = (attackDamage atk) * unoptimalDamage }

maybeAttack :: SoldierOutEvent -> Maybe AttackData
maybeAttack (AttackEvent atk) = Just atk
-- maybeAttack _ = Nothing

maybeAttacked :: SoldierInEvent -> Maybe (Double, V3 Double)
maybeAttacked (AttackedEvent dmg org) = Just (dmg, org)
-- maybeAttacked _                   = Nothing
