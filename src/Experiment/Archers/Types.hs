{-# OPTIONS -fno-warn-orphans #-}

module Experiment.Archers.Types where

-- import Physics
import Control.Monad.Random
import Linear.V2
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Colour
import Linear.V3
import Linear.Vector
import Prelude hiding ((.),id)
import Control.Wire
import Render.Sprite
import Render.Surface

data Stage = Stage { stageWidth :: Double
                   , stageHeight :: Double
                   , stageArchers :: [Archer]
                   , stageDarts :: [Dart]
                   }

type Angle = Double

data Archer = Archer  { archerPos     :: V3 Double
                      , archerHealth  :: Double
                      , archerAngle   :: Angle
                      , archerFlag    :: Maybe TeamFlag
                      } deriving Show

data Dart   = Dart    { dartPos     :: V3 Double
                      , dartDamage  :: Double
                      , dartAngle   :: Angle
                      } deriving Show

data TeamFlag = TeamFlag { teamFlagColor :: Color } deriving (Show, Eq)

data Team =  Team TeamFlag [Archer] [Dart]

data Message = Die | Hit Double

type Messages = [Message]

type Wire' = Wire (Timed Double ()) () Identity

type DartData = ((V3 Double,V3 Double), Double)

data ArcherData = ArcherData (V3 Double) Int (Maybe TeamFlag)

isDie :: Message -> Bool
isDie Die = True

maybeHit :: Message -> Maybe Double
maybeHit (Hit d) = Just d
maybeHit _       = Nothing

instance HasSurface Dart where
  toSurface (Dart (V3 x y _) _ ang) =
    Surface (V2 x y) (transRotate ang) [EntSprite spr]
    where
      spr = Sprite zero (Line (V2 (-2) 0) (V2 2 0)) black

instance HasSurface Archer where
  toSurface (Archer (V3 x y _) health ang flag) =
    Surface (V2 x y) (transRotate ang) [EntSprite spr]
    where
      col = maybe white teamFlagColor flag
      spr = Sprite zero (Circle 3 Filled) (health `darken` col)

-- mapTup :: (a -> b) -> (a,a,a) -> (b,b,b)
-- mapTup f (x,y,z) = (f x, f y, f z)


instance HasSurface Stage where
  toSurface (Stage w h arcs arrs) =
      Surface zero idTrans ents
    where
      back  = Sprite (V2 (w/2) (h/2)) (Rectangle (V2 w h) Filled) (sRGB24 1 142 14)
      arrEnts = map (EntSurface . toSurface) arrs
      arcEnts = map (EntSurface . toSurface) arcs
      ents = EntSprite back:(arcEnts ++ arrEnts)

instance (Random x, Random y) => Random (x, y) where
  randomR ((x1, y1), (x2, y2)) gen1 =
    let (x, gen2) = randomR (x1, x2) gen1
        (y, gen3) = randomR (y1, y2) gen2
    in ((x, y), gen3)
  random gen1 =
    let (x, gen2) = random gen1
        (y, gen3) = random gen2
    in ((x, y), gen3)

