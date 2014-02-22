module Experiment.Battlefield.Soldier (soldierWire) where

import Control.Monad
import Control.Monad.Fix
import Control.Wire                  as W
import Control.Wire.Unsafe.Event
import Data.List                     (minimumBy, mapAccumL)
import Data.Maybe                    (mapMaybe)
import Data.Ord                      (comparing)
import Experiment.Battlefield.Attack
import Experiment.Battlefield.Stats
import Experiment.Battlefield.Types
import FRP.Netwire.Move
import Linear
import Prelude hiding                ((.),id)
import Utils.Helpers                 (foldAcrossl)
import Utils.Wire.Misc
import Utils.Wire.Noise
import Utils.Wire.Wrapped

soldierWire :: (MonadFix m, HasTime t s, Monoid e, Fractional t)
    => SoldierData
    -> Wire s e m ([Soldier], SoldierInEvents) ((Soldier,[Article]), (SoldierOutEvents,[SoldierInEvents]))
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
    let
      shotW = map attackWire <$> shot

    rec
      atks    <- dWireBox' NoEvent -< (shotW, atkDies)
      let
        atkHits = checkAttacks atks targets
        atkDies = map (mconcat . map (() <$)) atkHits
        atkOuts = foldAcrossl (<>) mempty atkHits



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

    outE <- never -< ()
    returnA -< ((soldier,atks),(outE,atkOuts))

  where
    newAtk p (tDist, tDir)
      | tDist > range = Nothing
      | otherwise     = Just $ AttackData aX0 tDir . flip (Attack weap) aX0
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
        coupleRandom = couple (noisePrimR (1/damageVariance,damageVariance) gen)
        applyRandom (atk,r) = [atk (r * baseDamage)]
    checkAttacks :: [Article] -> [Soldier] -> [[SoldierInEvents]]
    checkAttacks atks sldrs = map makeKill atks
      where
        makeKill :: Article -> [SoldierInEvents]
        makeKill (Article (PosAng pa _) (ArticleAttack (Attack _ dmg o))) = snd $ mapAccumL f False sldrs
          where
            f True _                              = (True , NoEvent )
            f False sldr
              | norm (pa ^-^ soldierPos sldr) < 5 = (True , Event [AttackedEvent dmg o])
              | otherwise                         = (False, NoEvent )
