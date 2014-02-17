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

data Team = Team { teamFlag     :: TeamFlag
                 , teamSoldiers :: [Soldier]
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
                     } deriving Show

data AttackData = AttackData { attackDataX0     :: V3 Double
                             , attackDataDir    :: V3 Double
                             , attackDataAttack :: Attack
                             } deriving Show

type SoldierOutEvents = Event [SoldierOutEvent]

data SoldierInEvent = AttackedEvent Double
                        deriving Show

type SoldierInEvents = Event [SoldierInEvent]

data SoldierData = SoldierData { soldierDataX0     :: V3 Double
                               , soldierDataFlag   :: Maybe TeamFlag
                               , soldierDataBody   :: SoldierBody
                               , soldierDataWeapon :: Weapon
                               , soldierDataMount  :: Mount
                               , soldierDataGen    :: StdGen
                               } deriving Show
