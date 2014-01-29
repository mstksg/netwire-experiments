module Main where

import Control.Category
import Control.Wire
import FRP.Netwire
import Linear.V2
import Linear.V3
import Physics
import Prelude hiding   ((.),id)
import Render.Surface

-- data Team = Team { teamArcher :: Archer
--                  , teamArrows :: Arrow }

data Stage = Stage { stageHeight :: Int
                   , stageWidth :: Int
                   , stageArchers :: [Archer]
                   , stageArrows :: [AArrow]
                   }

data Archer = Archer Body

data AArrow = AArrow Body

main :: IO ()
main = return ()

simpleStage :: (Monad m, HasTime t s) => Int -> Int -> Wire s e m () Stage
simpleStage h w = Stage h w [] . return <$> arrow x0 v0
  where
    x0 = V3 (fromIntegral w) (fromIntegral h/2) 0
    v0 = V3 (-1) 0 0

arrow :: (Monad m, HasTime t s) => V3D -> V3D -> Wire s e m () AArrow
arrow x0 v0 = AArrow . Body 1 <$> proc _ -> do
  pos <- integral x0 -< v0
  returnA -< pos

-- instance SpriteClass AArrow where
--   toSprite (AArrow (Body _ (V3 x y _))) =
--     toSprite (SpritePrim (Circle 1) (V2 x y) (255,255,255))
