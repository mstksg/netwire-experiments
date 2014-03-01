module Experiment.Battlefield.Soldier
  ( soldierWire
  , swordsmanClass
  , archerClass
  , axemanClass
  , longbowmanClass
  , horsemanClass
  , horsearcherClass
  , allClasses
  , classStats
  , classWorth
  , normedClassWorths
  ) where

-- import Control.Wire.Unsafe.Event
-- import Data.Fixed                 (mod')
-- import Data.Foldable              (fold)
-- import Data.Traversable           (mapAccumL)
-- import Utils.Helpers              (rotationDir)
-- import Utils.Wire.Wrapped
import Control.Monad
import Control.Monad.Fix
import Control.Wire                  as W
import Data.List                     (minimumBy)
import Data.Maybe                    (mapMaybe, fromMaybe)
import Data.Ord                      (comparing)
import Experiment.Battlefield.Attack
import Experiment.Battlefield.Stats
import Experiment.Battlefield.Types
import FRP.Netwire.Move
import FRP.Netwire.Noise
import Linear
import Prelude hiding                ((.),id)
import System.Random
import Utils.Wire.Misc
import Utils.Wire.Noise
import qualified Data.Map.Strict     as M

type SoldierWireIn k = ((M.Map k Soldier,[Base]), SoldierInEvents)
type SoldierWireOut k = (Soldier, SoldierOutEvents)

type SoldierWire s e m k = Wire s e m (SoldierWireIn k) (SoldierWireOut k)


soldierWire :: forall s e m k. (MonadFix m, HasTime Double s, Monoid e, Ord k)
    => SoldierData
    -> SoldierWire s e m k
soldierWire (SoldierData x0 fl cls@(SoldierClass bod weap mnt) gen) =
  proc ((targets,targetBases),mess) -> do

    -- it's good to be alive!
    age <- integral 0 -< 1

    -- calculate damage from hits
    let hit = sum . map fst <$> attackeds
        attackeds = mapMaybe maybeAttacked <$> mess
        gotKill = sum . mapMaybe (fmap (const 1) . maybeGotKill) <$> mess
    damage <- hold . accumE (+) 0 <|> pure 0 -< hit

    -- calculate health, plus recovery
    recov <- integralWith (\d a -> min d a) 0 -< (recovery, damage)
    let health = maxHealth + recov - damage
        alive = health > 0

    (posAng,newD) <- moveAndAttack maaGen -< (targets,targetBases,attackeds,alive)

    -- shoot!
    shot  <- shoot -< newD

    killCount <- hold . accumE (+) 0 <|> 0 -< gotKill


    let -- hasAtks = not (null atks)

        wouldKill = (>= health) . attackDamage
        funcs     = SoldierFuncs wouldKill
        score     = SoldierScore killCount age

        soldier       = Soldier
                          posAng
                          (health / maxHealth)
                          fl score funcs bod weap mnt

    -- inhibit when dead
    W.when id -< alive

    -- outE <- never -< ()
    returnA -< (soldier, map AttackEvent <$> shot)

  where
    SoldierStats _ maxHealth baseDamage speed coolDown range' classAcc = classStats cls
    (gen',accGen') = split gen
    (dmgGen,maaGen) = split gen'
    (firstAcc,accGen) = randomR (-accLimit,accLimit) accGen'
    accLimit = atan $ (hitRadius * 1.1 / classAcc / 2) / range
    range = fromMaybe 7.5 range'
    isRanged = weaponRanged weap
    -- angSpeed = 2 * pi * 2.5
    accuracy
      | isRanged  = Just <$> (hold . noiseR coolDown (-accLimit,accLimit) accGen <|> pure firstAcc)
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
    recovery = maxHealth / recoveryFactor
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
              shot <- W.for coolDown . now -< a
              returnA -< shot
          ) --> oneShot
        coupleRandom = couple (noisePrimR (1/damageVariance,damageVariance) dmgGen)
        applyRandom (atk,r) = [atk (r * baseDamage)]

    moveAndAttack g = proc (targets,targetBases,attackeds,alive) -> do
      favoriteSpot <- arr (^* (baseRadius * 0.5)) . noiseDisc 1 0 g -< ()

      let
        targetsPos = map getPos (M.elems targets)
        basesPos = map ((^+^ favoriteSpot) . basePos) targetBases

      attackers <- curated -< (map snd <$> attackeds, findAttacker targetsPos)


      -- seeking and movement
      acc <- accuracy -< ()
      rec
        let
          zone | null basesPos = Nothing
               | otherwise     = fst <$> findClosest basesPos pos

          zone' = (first ((if noAttackers then id else const True) . (< (range + baseRadius * 0.5)))) <$> zone

          noAttackers = null attackers

          inZone = fromMaybe True $ fst <$> zone'

          -- inZone =
          --   case zone of
          --     Just ((zDist,_),_)
          --       | zDist < baseRadius * 0.85 -> True
          --       | otherwise -> False
          --     Nothing -> False

          -- find the target and the direction to face
          -- targetPool =
            -- case (attackers,targetBases,isRanged) of
            --   ([],[],False) -> targetsPos
            --   ([],[],True) -> targetsPos
            --   (_,[],False) -> targetsPos
            --   (_,[],True) -> attackers
            --   ([],_,False) -> basesPos
            --   ([],_,True) -> basesPos
            --   (_,_,False) -> attackers ++ basesPos
            --   (_,_,True) -> attackers

          targetPool  | noAttackers || not isRanged = targetsPos
                      | otherwise                   = attackers
          target = guard inZone >> seek targetPool pos
          target' = (first (> range)) <$> target
          newD
            | alive     = target >>= newAtk pos acc
            | otherwise = Nothing

          -- move to target?
          (dir,vel) =
            case (target',zone') of
              (Just (True, tDir),_) -> (Just tDir, tDir ^* speed)
              (_,Just (False, bDir)) -> (Just bDir, bDir ^* speed)
              _                  -> (Nothing, zero)

        pos <- integral x0 -< vel

      -- calculate last direction facing.  holdJust breaks FRP.
      (V3 vx vy _) <- holdJust zero -< dir

      returnA -< (PosAng pos (atan2 vy vx),newD)

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

allClasses :: [SoldierClass]
allClasses = [ swordsmanClass, archerClass, axemanClass
             , longbowmanClass, horsemanClass, horsearcherClass]

classStats :: SoldierClass -> SoldierStats
classStats (SoldierClass bod weap mnt) = SoldierStats dps hlt dmg spd cld rng acc
  where
    dps = weaponDPS weap * mountDamageMod mnt / acc
    hlt = bodyHealth bod * mountHealthMod mnt
    dmg = dps * cld
    spd = mountSpeed mnt * bodySpeedMod bod
    cld = weaponCooldown weap
    rng = weaponRange weap
    acc = fromMaybe 1 (rangedAccuracy <$ rng)

classWorth :: SoldierClass -> Double
classWorth = statsWorth . classStats
  where
    statsWorth (SoldierStats dps hlt dmg spd _ rng _) = sum statZipped - shunter
      where
        shunter    = 2.5
        statArr    = [ dps , hlt , spd , dmg , fromMaybe 0   rng ]
        statNorm   = [ 5   , 25  , 33  , 5   , 25              ]
        statWeight = [ 1.85, 1.67, 0.75, 0.5 , 1.5              ]
        statZipped = zipWith3 mul3 statArr statNorm statWeight
        mul3 a n z = (a/n)*z

normedClassWorths :: M.Map SoldierClass Double
normedClassWorths = M.fromList $ zip allClasses normed
  where
    worths = map classWorth allClasses
    worthsum = sum worths
    normed = map (/ worthsum) worths
