
module Experiment.Planets.Types where

import Data.Word
import Linear.V2
import Linear.V3
import Linear.Vector
import Physics
import Render.Sprite
import Render.Surface

data Planet = Planet  { planetName    :: String
                      , planetRadius  :: Double
                      , planetColor   :: (Word8,Word8,Word8)
                      , planetBody    :: Body
                      } deriving (Show)

newtype PlanetList = PlanetList [Planet]

instance HasSprite Planet where
  toSprite (Planet _ r c (Body _ (V3 x y _))) =
    Sprite (V2 x y) (Circle r Filled) c

instance HasSurface PlanetList where
  toSurface (PlanetList ps) =
    Surface zero idTrans (map (EntSprite . toSprite) ps)


