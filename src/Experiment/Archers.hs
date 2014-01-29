module Main where

import Control.Category
import Control.Wire
import FRP.Netwire
import Render.Render
import Linear.V2
import Linear.V3
import Linear.Vector
import Physics
import Prelude hiding            ((.),id)
import Render.Backend.SDL
import Render.Sprite
import Render.Surface
import qualified Graphics.UI.SDL as SDL

-- data Team = Team { teamArcher :: Archer
--                  , teamArrows :: Arrow }

data Stage = Stage { stageWidth :: Double
                   , stageHeight :: Double
                   , stageArchers :: [Archer]
                   , stageArrows :: [AArrow]
                   }

data Archer = Archer Body
data AArrow = AArrow Body

instance HasSprite AArrow where
  toSprite (AArrow (Body _ (V3 x y _))) =
    Sprite (V2 x y) (Circle 5) (0,0,0)

instance HasSurface Stage where
  toSurface (Stage _ _ _ as) = Surface zero idTrans (map toSprite as) []

instance SDLRenderable Stage where
  renderSDL scr stg@(Stage w h _ _) = mapM_ (renderSDL scr) sList
    where
      sList = toSpriteList zero (transScale scale) (toSurface stg)
      ht    = fromIntegral $ SDL.surfaceGetHeight scr
      wd    = fromIntegral $ SDL.surfaceGetHeight scr
      scale = min (ht / h) (wd / w)

main :: IO ()
main = testStage (simpleStage 400 300)

simpleStage :: (Monad m, HasTime t s) => Double -> Double -> Wire s e m () Stage
simpleStage w h = Stage w h [] . return <$> arrow x0 v0
  where
    x0 = V3 w (h/2) 0
    v0 = V3 (-1) 0 0

arrow :: (Monad m, HasTime t s) => V3D -> V3D -> Wire s e m () AArrow
arrow x0 v0 = AArrow . Body 1 <$> proc _ -> do
  pos <- integral x0 -< v0
  returnA -< pos

testStage :: Wire (Timed Double ()) e IO () Stage -> IO ()
testStage w =
  runBackend
    (sdlBackend 600 600 (250,250,250))
    (const . return . return $ ())
    (w . pure ())




-- runTestSDL :: Wire (Timed Double ()) String IO (Event RenderEvent) [Planet] -> IO ()
-- runTestSDL w =
--   runBackend (sdlBackend 600 600 (31,31,31)) (const . return . return $ ()) (PlanetList <$> w)
