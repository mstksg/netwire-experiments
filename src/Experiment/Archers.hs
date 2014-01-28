module Main where

import Prelude hiding ((.),id)
import Physics
import Control.Category
import FRP.Netwire
import Control.Wire


-- data Team = Team { teamArcher :: Archer 
--                  , teamArrows :: Arrow }

data Archer = Archer Body

data AArrow = AArrow Body

main :: IO ()
main = return ()

arrow :: (Monad m, HasTime t s) => V3D -> V3D -> Wire s e m () AArrow
arrow x0 v0 = AArrow . Body 1 <$> proc _ -> do
  pos <- integral x0 -< v0
  returnA -< pos
