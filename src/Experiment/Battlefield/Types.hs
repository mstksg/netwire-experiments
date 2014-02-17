module Experiment.Battlefield.Types where

import Render.Sprite
import Control.Wire
import Linear.V3

data Stage = Stage { stageWidth    :: Double
                   , stageHeight   :: Double
                   , stageSoldiers :: [Soldier]
                   , stageArticles :: [Article]
                   } deriving Show

data Soldier = Soldier  { soldierPosAng :: PosAng
                        , soldierHealth :: Double
                        , soldierFlag   :: Maybe TeamFlag
                        , soldierBody   :: SoldierBody
                        , soldierWeapon :: SoldierWeapon
                        , soldierMount  :: SoldierMount
                        } deriving Show

data SoldierBody = MeleeBody | TankBody | RangedBody
                     deriving Show

data SoldierWeapon = Sword | Axe | Bow | Longbow
                       deriving Show

data SoldierMount = Foot | Horse
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

data SoldierOutEvent = MeleeEvent | DartEvent

type SoldierOutEvents = Event [SoldierOutEvent]

data SoldierInEvent = HitEvent

type SoldierInEvents = Event [SoldierInEvent]


