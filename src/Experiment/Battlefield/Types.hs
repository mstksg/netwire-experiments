module Experiment.Battlefield.Types where

import Render.Surface

data Stage = Stage { stageWidth    :: Double
                   , stageHeight   :: Double
                   , stageEntities :: [Entity]
                   }

data Soldier = Soldier

