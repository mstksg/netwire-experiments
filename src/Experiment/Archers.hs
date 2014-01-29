{-# LANGUAGE TupleSections #-}

module Main where

import Control.Category
import Control.Wire              as W
import FRP.Netwire
import Linear.V2
import Linear.V3
import Linear.Vector
import Physics
import Prelude hiding            ((.),id)
import Render.Backend.SDL
import Render.Render
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

type Angle = Double

data Archer = Archer Body Angle
data AArrow = AArrow Body Angle

-- instance HasSprite AArrow where
--   toSprite (AArrow (Body _ (V3 x y _)) _) =

instance HasSurface AArrow where
  toSurface (AArrow (Body _ (V3 x y _)) ang) =
    Surface (V2 x y) (transRotate ang) [EntSprite spr]
    where
      spr = Sprite zero (Line (V2 (-2) 0) (V2 2 0)) (0,0,0)

instance HasSurface Archer where
  toSurface (Archer (Body _ (V3 x y _)) ang) =
    Surface (V2 x y) (transRotate ang) [EntSprite spr]
    where
      spr = Sprite zero (Circle 3 Unfilled) (0,0,0)

instance HasSurface Stage where
  toSurface (Stage w h arcs arrs) =
      Surface zero idTrans ents
    where
      back  = Sprite (V2 (w/2) (h/2)) (Rectangle (V2 w h) Filled) (1,142,14)
      arrEnts = map (EntSurface . toSurface) arrs
      arcEnts = map (EntSurface . toSurface) arcs
      ents = EntSprite back:(arcEnts ++ arrEnts)

instance SDLRenderable Stage where
  renderSDL scr stg@(Stage w h _ _) = mapM_ (renderSDL scr) sList
    where
      sList = toSpriteList zero (transScale scale) (toSurface stg)
      ht    = fromIntegral $ SDL.surfaceGetHeight scr
      wd    = fromIntegral $ SDL.surfaceGetWidth scr
      scale = min (ht / h) (wd / w)

main :: IO ()
main = testStage (simpleStage 400 300)

simpleStage :: (Monad m, HasTime t s, Monoid e, Fractional t) => Double -> Double -> Wire s e m () Stage
simpleStage w h = proc _ -> do
    arws <- return <$> arrow x0 v0 . at 7 --> pure [] -< Hit
    arcs <- return <$> archer a0 . at 5 --> pure [] -< Hit
    returnA -< Stage w h arcs arws
  where
    x0 = V3 w (h/2) 0
    v0 = V3 (-12) 0 0
    a0 = V3 (w/2) (h/2) 0

data Hit = Hit

arrow :: forall s t e m. (Monad m, HasTime t s, Monoid e) => V3D -> V3D -> Wire s e m (Event Hit) AArrow
arrow x0 v0@(V3 vx vy _) = hittable arrow' . arr ((),)
  where
    arrow' :: Wire s e m () AArrow
    arrow' = proc _ -> do
      pos <- integral x0 -< v0
      returnA -< AArrow (Body 1 pos) (atan2 vy vx)

archer :: forall s t e m. (Monad m, Monoid e, HasTime t s, Fractional t) => V3D -> Wire s e m (Event Hit) Archer
archer x0 = hittable archer' . arr ((),)
  where
    archer' :: Wire s e m () Archer
    archer' = proc _ -> do
      vx <- hold . stdNoiseR 2 (-10,10) 1 -< ()
      vy <- hold . stdNoiseR 3 (-10,10) 2 -< ()
      pos <- integral x0 -< V3 vx vy 0
      returnA -< Archer (Body 1 pos) (atan2 vy vx)


hittable :: (Monad m, Monoid e) => Wire s e m a b -> Wire s e m (a, Event Hit) b
hittable wr = proc (a,h) -> do
  x <- wr -< a
  alive <- W.until -< (x, h)
  returnA -< alive

testStage :: Wire (Timed Double ()) () IO () Stage -> IO ()
testStage w =
  runBackend
    (sdlBackend 600 600 (50,50,50))
    (const . return . return $ ())
    (w . pure ())




-- runTestSDL :: Wire (Timed Double ()) String IO (Event RenderEvent) [Planet] -> IO ()
-- runTestSDL w =
--   runBackend (sdlBackend 600 600 (31,31,31)) (const . return . return $ ()) (PlanetList <$> w)
