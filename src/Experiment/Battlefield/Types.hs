module Experiment.Battlefield.Types where

import Render.Sprite
import Control.Wire
import Prelude hiding ((.),id)
import Data.Default
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Render.Surface
import Linear
import System.Random

type Wire' = Wire (Timed Double ()) () Identity

data Stage = Stage { stageDimensions :: (Double,Double)
                   , stageSoldiers   :: [Soldier]
                   , stageArticles   :: [Article]
                   } deriving Show

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
                     deriving Show

data Weapon = Sword | Axe | Bow | Longbow
                       deriving Show

data Mount = Foot | Horse
                      deriving Show

data Team = Team { teamFlag     :: TeamFlag
                 , teamSoldiers :: [Maybe Soldier]
                 , teamArticles :: [Article]
                 } deriving Show

data TeamFlag = TeamFlag  { teamFlagColor :: Color
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
                                    } deriving Show

type SoldierInEvents = Event [SoldierInEvent]

data TeamInEvent = GotBase Base

type TeamInEvents = Event [TeamInEvent]

data BaseEvent = LoseBase

data Base = Base { basePos :: V3 Double }

data SoldierData = SoldierData { soldierDataX0     :: V3 Double
                               , soldierDataFlag   :: Maybe TeamFlag
                               , soldierDataClass  :: SoldierClass
                               , soldierDataGen    :: StdGen
                               } deriving Show

data SoldierClass = SoldierClass { soldierClassBody   :: SoldierBody
                                 , soldierClassWeapon :: Weapon
                                 , soldierClassMount  :: Mount
                                 } deriving Show

data SoldierStats = SoldierStats { soldierStatsDPS      :: Double
                                 , soldierStatsHealth   :: Double
                                 , soldierStatsDamage   :: Double
                                 , soldierStatsSpeed    :: Double
                                 , soldierStatsCooldown :: Double
                                 , soldierStatsRange    :: Maybe Double
                                 , soldierStatsAccuracy :: Double
                                 }

instance HasSurface Stage where
  toSurface (Stage (w,h) sldrs arts) = Surface zero idTrans 1 ents
    where
      back  = Sprite (V2 (w/2) (h/2)) (Rectangle (V2 w h) Filled) (sRGB24 126 126 126) 1
      sldrEnts = map (EntSurface . toSurface) sldrs
      artEnts = map (EntSurface . toSurface) arts
      ents = EntSprite back:(sldrEnts ++ artEnts)

instance HasSurface Soldier where
  toSurface (Soldier (PosAng (V3 x y _) ang) health fl _ _ _ weap mnt) =
      Surface (V2 x y) (transRotate ang) 1 [mountEnt,bodyEnt,weaponEnt]
    where
      mountEnt  =
        case mnt of
          Foot  -> EntSurface $ Surface zero idTrans 1 $ map foot [-1.5,1.5]
            where
              foot y' = EntSprite $ Sprite (V2 1 y') (Circle 0.5 Filled) brown 1
          Horse -> EntSprite $ Sprite zero (Ellipse (V2 5 2) Filled) brown 1
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

instance Default SoldierScore where
  def = SoldierScore 0 0

instance Default SoldierFuncs where
  def = SoldierFuncs (const False)

soldierPos :: Soldier -> V3 Double
soldierPos = posAngPos . soldierPosAng

