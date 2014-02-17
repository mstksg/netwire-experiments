
module Experiment.Planets.Types where

-- import Data.Word
import Linear.V2
import Linear.V3
import Linear.Vector
import Physics
import Render.Sprite
import Render.Surface
-- import Data.Colour

data Planet = Planet  { planetName    :: String
                      , planetRadius  :: Double
                      , planetColor   :: Color
                      , planetBody    :: Body
                      } deriving (Show)

data PlanetList = PlanetList { planetListZoom :: Double
                             , planetListPlanets :: [Planet]
                             } deriving (Show)

instance HasSprite Planet where
  toSprite (Planet _ r c (Body _ (V3 x y _))) =
    Sprite (V2 x y) (Circle r Filled) c

instance HasSurface PlanetList where
  toSurface (PlanetList z ps) =
    Surface zero (transScale z) (map (EntSprite . toSprite) ps)


