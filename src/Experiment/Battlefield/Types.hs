module Experiment.Battlefield.Types where

import Render.Sprite
import Control.Wire
import Linear.V3
import System.Random

data Stage = Stage { stageWidth    :: Double
                   , stageHeight   :: Double
                   , stageSoldiers :: [Soldier]
                   , stageArticles :: [Article]
                   } deriving Show

data Soldier = Soldier  { soldierPosAng :: PosAng
                        , soldierHealth :: Double
                        , soldierFlag   :: Maybe TeamFlag
                        , soldierBody   :: SoldierBody
                        , soldierWeapon :: Weapon
                        , soldierMount  :: Mount
                        } deriving Show

data SoldierBody = MeleeBody | TankBody | RangedBody
                     deriving Show

data Weapon = Sword | Axe | Bow | Longbow
                       deriving Show

data Mount = Foot | Horse
                      deriving Show

data TeamFlag = TeamFlag  { teamFlagColor :: Color
                          } deriving Show

data Article = Article { articlePosAng :: PosAng
                       , articleType   :: ArticleType
                       } deriving Show

data ArticleType = Dart deriving Show

data PosAng = PosAng { posAngPos :: V3 Double
                     , posAngAng :: Double
                     } deriving Show

data SoldierOutEvent = AttackEvent { attackEventPos    :: V3 Double
                                   , attackEventDir    :: V3 Double
                                   , attackEventWeapon :: Weapon
                                   , attackEventDamage :: Double
                                   } deriving Show

type SoldierOutEvents = Event [SoldierOutEvent]

data SoldierInEvent = HitEvent

type SoldierInEvents = Event [SoldierInEvent]

data SoldierData = SoldierData { soldierDataX0     :: V3 Double
                               , soldierDataFlag   :: Maybe TeamFlag
                               , soldierDataBody   :: SoldierBody
                               , soldierDataWeapon :: Weapon
                               , soldierDataMount  :: Mount
                               , soldierDataGen    :: StdGen
                               }
