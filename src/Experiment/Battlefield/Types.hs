{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Experiment.Battlefield.Types where

import Control.Wire
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Default
import Data.Ord
import Linear
import Prelude hiding            ((.),id)
import Render.Sprite
import Render.Surface
import System.Random
import qualified Data.Map.Strict as M

type Wire' = Wire (Timed Double ()) String Identity

newtype UUID = UUID { getUUID :: Int } deriving (Show, Ord, Eq, Enum)

uuids :: [UUID]
uuids = [(UUID 0)..]

data Stage = Stage { stageDimensions :: (Double,Double)
                   , stageScore      :: StageScore
                   , stageSoldiers   :: [Soldier]
                   , stageArticles   :: [Article]
                   , stageBases      :: [Base]
                   } deriving Show

data StageScore = StageScore { stageScoreScores    :: (Int,Int)
                             , stageScoreGameCount :: Int
                             , stageScoreDuration  :: Double
                             } deriving Show

data Hittable = HittableSoldier Soldier
              | HittableBase Base

data Soldier = Soldier  { soldierPosAng :: PosAng
                        , soldierHealth :: Double
                        , soldierFlag   :: Maybe TeamFlag
                        , soldierScore  :: SoldierScore
                        , soldierFuncs  :: SoldierFuncs
                        , soldierBody   :: SoldierBody
                        , soldierWeapon :: Weapon
                        , soldierMount  :: Mount
                        } deriving Show


data SoldierScore = SoldierScore { soldierScoreKillCount :: Int
                                 , soldierScoreAge       :: Double
                                 } deriving Show

data SoldierFuncs = SoldierFuncs { soldierFuncsWouldKill :: Attack -> Bool
                                 }

data SoldierBody = MeleeBody | TankBody | RangedBody
                     deriving (Show, Ord, Eq)

data Weapon = Sword | Axe | Bow | Longbow
                       deriving (Show, Ord, Eq)

data Mount = Foot | Horse
                      deriving (Show, Ord, Eq)

data Team = Team { teamFlag     :: TeamFlag
                 , teamSoldiers :: M.Map UUID Soldier
                 , teamBases    :: [Base]
                 } deriving Show

data TeamFlag = TeamFlag  { teamFlagColor :: Color
                          } deriving (Show, Eq, Ord)

data TeamData = TeamData { teamDataFlag :: TeamFlag
                         , teamDataGen  :: StdGen
                         } deriving Show

data Article = Article { articlePosAng :: PosAng
                       , articleType   :: ArticleType
                       } deriving Show

data ArticleType = ArticleAttack Attack deriving Show

data PosAng = PosAng { posAngPos :: V3 Double
                     , posAngAng :: Double
                     } deriving Show

data SoldierOutEvent = AttackEvent AttackData

data Attack = Attack { attackWeapon :: Weapon
                     , attackDamage :: Double
                     , attackOrigin :: V3 Double
                     } deriving Show

data AttackData = AttackData { attackDataX0     :: V3 Double
                             , attackDataDir    :: V3 Double
                             , attackDataAttack :: Attack
                             } deriving Show

type SoldierOutEvents = Event [SoldierOutEvent]

data SoldierInEvent = AttackedEvent { attackedEventDamage :: Double
                                    , attackedEventOrigin :: V3 Double
                                    }
                    | GotKillEvent Soldier
                    deriving Show

type SoldierInEvents = Event [SoldierInEvent]

data TeamInEvent

data BaseEvent = GetBase | LoseBase (Maybe TeamFlag)

type BaseEvents = Event [BaseEvent]

data Base = Base { basePos      :: V3 Double
                 , baseTeamFlag :: Maybe TeamFlag
                 , baseSecurity :: Double
                 , baseLeaning  :: Maybe TeamFlag
                 , baseWall     :: Maybe Double
                 } deriving Show

baseRadius :: Double
baseRadius = 25

data SoldierData = SoldierData { soldierDataX0     :: V3 Double
                               , soldierDataFlag   :: Maybe TeamFlag
                               , soldierDataClass  :: SoldierClass
                               , soldierDataGen    :: StdGen
                               } deriving Show

data SoldierClass = SoldierClass { soldierClassBody   :: SoldierBody
                                 , soldierClassWeapon :: Weapon
                                 , soldierClassMount  :: Mount
                                 } deriving (Show, Ord, Eq)

data SoldierStats = SoldierStats { soldierStatsDPS      :: Double
                                 , soldierStatsHealth   :: Double
                                 , soldierStatsDamage   :: Double
                                 , soldierStatsSpeed    :: Double
                                 , soldierStatsCooldown :: Double
                                 , soldierStatsRange    :: Maybe Double
                                 , soldierStatsAccuracy :: Double
                                 }

backgroundColor :: Color
backgroundColor = sRGB24 126 126 126

instance (Ord a, Floating a) => Ord (Colour a) where
  compare = comparing toSRGB

instance Ord a => Ord (RGB a) where
  compare (RGB r1 g1 b1) (RGB r2 g2 b2) = compare (r1,g1,b1) (r2,g2,b2)

instance HasSurface Stage where
  toSurface (Stage (w,h) _ sldrs arts bases) = Surface zero idTrans 1 ents
    where
      back  = Sprite (V2 (w/2) (h/2)) (Rectangle (V2 w h) Filled) backgroundColor 1
      baseEnts = map (EntSurface . toSurface) bases
      sldrEnts = map (EntSurface . toSurface) sldrs
      artEnts = map (EntSurface . toSurface) arts
      ents = EntSprite back:(baseEnts ++ sldrEnts ++ artEnts)

instance HasSurface Base where
  toSurface (Base (V3 x y _) fl sec lean wall) = Surface (V2 x y) idTrans 1 [baseWallOutline,baseOutline,baseCirc]
    where
      wallColor = blend (maybe 0 ((+ 1/3) . (* (2/3))) wall) black backgroundColor
      baseWallOutline = EntSprite $ Sprite zero (Circle (baseRadius * 17/16) Filled) wallColor 1
      baseOutline = EntSprite $ Sprite zero (Circle baseRadius Filled) (blend (2/3) backgroundColor $ maybe white teamFlagColor fl) 1
      baseCirc = EntSprite $ Sprite zero (Circle (baseRadius * 15/16) Filled) col 1
      col = blend (3/4) backgroundColor $
        case (fl,lean) of
          (Just c,_)  ->
            sec `darken` teamFlagColor c
          (_,Nothing) ->
            white
          (_,Just c)  ->
            blend sec white (teamFlagColor c)

instance HasSurface Soldier where
  toSurface (Soldier (PosAng (V3 x y _) ang) health fl _ _ _ weap mnt) =
      Surface (V2 x y) (transRotate ang) 1 [mountEnt,bodyEnt,weaponEnt]
    where
      mountEnt  =
        case mnt of
          Foot  -> EntSurface $ Surface zero idTrans 1 $ map foot [-1.5,1.5]
            where
              foot y' = EntSprite $ Sprite (V2 1 y') (Circle 0.5 Filled) (sRGB24 52 52 52) 1
          Horse -> EntSprite $ Sprite zero (Ellipse (V2 5 2) Filled) (sRGB24 52 52 52) 1
      weaponEnt =
        case weap of
          Sword   -> EntSprite $ Sprite zero (Line (V2 0 1) (V2 4 1)) black 1
          Bow     -> EntSprite $ Sprite zero (Line (V2 2.5 2) (V2 2.5 (-2))) black 1
          Longbow -> EntSprite $ Sprite zero (Line (V2 2.5 4) (V2 2.5 (-4))) black 1
          Axe     -> EntSprite $ Sprite zero axePoly black 1
            where
              axePoly = Polygon [V2 0 0, V2 4 2, V2 2 4] Filled
      bodyEnt   = EntSprite $ Sprite zero bodyPoly bodyCol 1
      bodyPoly  = Polygon [V2 (-3) 2, V2 3 0, V2 (-3) (-2)] Filled
      baseCol   = maybe white teamFlagColor fl
      bodyCol   = health `darken` baseCol

instance HasSurface Article where
  toSurface (Article (PosAng (V3 x y _) ang) aType) =
      Surface (V2 x y) (transRotate ang) 1 [baseEnt aType]
    where
      baseEnt (ArticleAttack atk) =
        case attackWeapon atk of
          Sword   -> EntSprite $ Sprite zero (Circle 1 Filled) white 1
          Axe     -> EntSprite $ Sprite zero (Circle 2 Filled) yellow 2
          Bow     -> EntSprite $ Sprite zero (Line (V2 (-2) 0) (V2 2 0)) black 1
          Longbow -> EntSprite $ Sprite zero (Line (V2 (-2) 0) (V2 2 0)) black 1

instance Show SoldierFuncs where
  show _ = "Soldier Functions"

instance Default StageScore where
  def = StageScore (0,0) 0 0

instance Default Team where
  def = Team def M.empty []

instance Default TeamFlag where
  def = TeamFlag white

instance Default SoldierScore where
  def = SoldierScore 0 0

instance Default SoldierFuncs where
  def = SoldierFuncs (const False)

class HasPosAng a where
  getPosAng :: a -> PosAng

class HasPos a where
  getPos :: a -> V3 Double

instance HasPosAng Soldier where
  getPosAng = soldierPosAng

instance HasPosAng Article where
  getPosAng = articlePosAng

instance HasPos Base where
  getPos = basePos

instance HasPos PosAng where
  getPos = posAngPos

instance (HasPosAng a) => HasPos a where
  getPos = getPos . getPosAng

instance HasPos Hittable where
  getPos (HittableSoldier s) = getPos s
  getPos (HittableBase b) = getPos b

instance HasPos (V3 Double) where
  getPos = id

hitRadius :: Double
hitRadius = 5

hittableHit :: HasPos a => Hittable -> a -> Bool
hittableHit (HittableSoldier s) a = norm (getPos a ^-^ getPos s) < hitRadius
hittableHit (HittableBase b) a = d < 1.1 && d > 1
  where
    d = norm (getPos a ^-^ getPos b) / baseRadius
