{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Category
import Control.Monad
import Control.Wire.Unsafe.Event
import Control.Monad.Fix
import Control.Wire                     as W
import Data.Maybe                       (maybeToList, listToMaybe, catMaybes)
import Experiment.Archers.Types
import FRP.Netwire
import Linear.Metric
import Linear.V3
import Linear.Vector
import Physics
import Prelude hiding                   ((.),id)
import Render.Render

#ifdef WINDOWS
import Render.Backend.GLUT
import Experiment.Archers.Instances.GLUT    ()
#else
import Render.Backend.SDL
import Experiment.Archers.Instances.SDL ()
#endif



-- data Team = Team { teamArcher :: Archer
--                  , teamArrows :: Arrow }

main :: IO ()
main = testStage (simpleStage 400 300)

simpleStage :: forall m t s e. (MonadFix m, HasTime t s, Monoid e, Fractional t)
    => Double
    -> Double
    -> Wire s e m () Stage
simpleStage w h = proc _ -> do
    (as,ds) <- hitInteraction -< ()
    returnA -< Stage w h as ds
  where
    hitInteraction :: Wire s e m () ([Archer], [Dart])
    hitInteraction = proc _ -> do
      rec
        a1 <- archerWire a10 . delay NoEvent -< hita1
        a2 <- archerWire a20 . delay NoEvent -< hita2
        a3 <- archerWire a30 . delay NoEvent -< hita3
        d1 <- dartWire x10 v10 . delay NoEvent -< hitd1
        d2 <- dartWire x20 v20 . delay NoEvent -< hitd2
        d3 <- dartWire x30 v30 . delay NoEvent -< hitd3
        hit11 <- hitWire -< (a1,d1)
        hit12 <- hitWire -< (a1,d2)
        hit13 <- hitWire -< (a1,d3)
        hit21 <- hitWire -< (a2,d1)
        hit22 <- hitWire -< (a2,d2)
        hit23 <- hitWire -< (a2,d3)
        hit31 <- hitWire -< (a3,d1)
        hit32 <- hitWire -< (a3,d2)
        hit33 <- hitWire -< (a3,d3)
        hita1 <- arr mergeEs -< [ hit11, hit12, hit13 ]
        hita2 <- arr mergeEs -< [ hit21, hit22, hit23 ]
        hita3 <- arr mergeEs -< [ hit31, hit32, hit33 ]
        hitd1 <- arr mergeEs -< [ hit11, hit21, hit21 ]
        hitd2 <- arr mergeEs -< [ hit12, hit22, hit32 ]
        hitd3 <- arr mergeEs -< [ hit13, hit23, hit33 ]
      returnA -< (catMaybes [a1,a2,a3], catMaybes [d1,d2,d3])

    mergeEs :: [Event a] -> Event a
    mergeEs = foldl (merge const) NoEvent

    -- x0 = V3 w (h/2) 0
    -- v0 = V3 (-12) 0 0
    -- a0 = V3 (w/2) (h/2) 0
    x10 = V3 w (h/2) 0
    v10 = V3 (-12) 0 0
    a10 = V3 (w/2) (h/2) 0
    x20 = V3 (w*3/4) (h/2) 0
    v20 = V3 (-12) 0 0
    a20 = V3 (w/4) (h/2) 0
    x30 = V3 (w/2) 0 0
    v30 = V3 0 (12) 0
    a30 = V3 (w/2) (h*3/4) 0

-- sequenceHits :: Monad m
--     => [Wire s e m (Event Hit) (Maybe Archer)]
--     -> [Wire s e m (Event Hit) (Maybe Dart)]
--     -> Wire s e m () ([Archer],[Dart])
-- sequenceHits aWires dWires = undefined
--   where
--     archers = map (archerWire dWires) aWires
--     archerWire [] aWire = aWire . never . pure ()
--     archerWire (d:ds) aWire =

data Hit = Hit

archerWire :: (Monad m, Monoid e) => V3D -> Wire s e m (Event Hit) (Maybe Archer)
archerWire x0 = proc h -> do
  xa <- pure x0 -< ()
  W.until --> pure Nothing
    -< (Just (Archer (Body 1 xa) 0), h)

dartWire :: (Monad m, Monoid e, HasTime t s) => V3D -> V3D -> Wire s e m (Event Hit) (Maybe Dart)
dartWire x0 v0@(V3 vx vy _) = proc h -> do
  pos <- integral x0 -< v0
  W.until --> pure Nothing
    -< (Just (Dart (Body 1 pos) (atan2 vy vx)), h)

  -- e <-
  --   case a of
  --     Just (Archer (Body _ xa) _) ->
  --       never . W.unless ((< 5) . norm) . arr fst --> now . arr snd
  --         -< (pos ^-^ xa, Hit)
  --     Nothing ->
  --       never -< ()
  -- d <-
  --   arr Just . W.until --> pure Nothing
  --     -< (Dart (Body 1 pos) (atan2 vy vx), e)
  -- returnA -< (d,e)


hitWire :: (Monad m, Monoid e) => Wire s e m (Maybe Archer, Maybe Dart) (Event Hit)
hitWire = proc ad ->
  case ad of
    (Just (Archer (Body _ xa) _),Just (Dart (Body _ xd) _)) ->
      never . W.unless ((< 5) . norm) . arr fst --> now . arr snd
        -< (xa ^-^ xd, Hit)
    _ ->
      never
        -< ()


--         never . W.unless (\(d,_) -> norm d < 5) --> arr (snd<$>) . now
--           -< (arcpos ^-^ pos, Hit)

-- archersAndDarts :: Monad m => Wire s e m ([Archer],[Dart]) ([Archer],[Dart])
-- archersAndDarts = proc (as,ds) -> undefined -< undefined
--   where
--     dartAsWire :: Wire s e m (Dart, [Archer]) Dart
--     dartAsWire = undefined
--     archerDsWire :: Wire s e m (Archer, [Dart]) Archer
--     archerDsWire = undefined
--     dartWire :: Wire s e m (Dart, Archer) Dart
--     dartWire = undefined
--     archerWire :: Wire s e m (Archer, Dart) Archer
--     archerWire = undefined



-- tryReturn :: (MonadPlus mp, Monoid e, Monad m) => Wire s e m a b -> Wire s e m a (mp b)
-- tryReturn w = return <$> w <|> pure mzero

-- data Hit = Hit

-- dart :: forall s t e m. (Monad m, HasTime t s, Monoid e) => V3D -> V3D -> Wire s e m (Event Hit) Dart
-- dart x0 v0@(V3 vx vy _) = hittable dart' . arr ((),)
--   where
--     dart' :: Wire s e m () Dart
--     dart' = proc _ -> do
--       pos <- integral x0 -< v0
--       returnA -< Dart (Body 1 pos) (atan2 vy vx)

-- dartH :: forall s t e m. (Monad m, HasTime t s, Monoid e) => V3D -> V3D -> Wire s e m Archer (Dart, Event Hit)
-- dartH x0 v0@(V3 vx vy _) = dart'
--   where
--     dart' :: Wire s e m Archer (Dart, Event Hit)
--     dart' = proc (Archer (Body _ arcpos) _) -> do
--       pos <- integral x0 -< v0
--       hitArcher <-
--         never . W.unless (\(d,_) -> norm d < 5) --> arr (snd<$>) . now
--           -< (arcpos ^-^ pos, Hit)
--       returnA -< (Dart (Body 1 pos) (atan2 vy vx),hitArcher)

-- dartHM :: forall s t e m. (Monad m, HasTime t s, Monoid e) => V3D -> V3D -> Wire s e m Archer (Maybe Dart, Event Hit)
-- dartHM x0 v0@(V3 vx vy _) = dart'
--   where
--     dart' :: Wire s e m Archer (Maybe Dart, Event Hit)
--     dart' = proc (Archer (Body _ arcpos) _) -> do
--       pos <- integral x0 -< v0
--       hitArcher <-
--         never . W.unless (\(d,_) -> norm d < 5) --> arr (snd<$>) . now
--           -< (arcpos ^-^ pos, Hit)
--       let
--         d = Dart (Body 1 pos) (atan2 vy vx)
--       dartStill <- Nothing <$ hold <|> pure (Just ()) -< hitArcher
--       let
--         dart'' = d <$ dartStill
--       returnA -< (dart'',hitArcher)

-- dartHH :: forall s t e m. (Monad m, HasTime t s, Monoid e) => V3D -> V3D -> Wire s e m (Archer, Event Hit) (Dart, Event Hit)
-- dartHH x0 v0@(V3 vx vy _) = hittable dart'
--   where
--     dart' :: Wire s e m Archer (Dart, Event Hit)
--     dart' = proc (Archer (Body _ arcpos) _) -> do
--       pos <- integral x0 -< v0
--       hitArcher <-
--         never . W.unless (\(d,_) -> norm d < 5) --> arr (snd<$>) . now
--           -< (arcpos ^-^ pos, Hit)
--       returnA -< (Dart (Body 1 pos) (atan2 vy vx),hitArcher)

-- archer :: forall s t e m. (Monad m, Monoid e, HasTime t s, Fractional t) => V3D -> Wire s e m (Event Hit) Archer
-- archer x0 = hittable archer' . arr ((),)
--   where
--     archer' :: Wire s e m () Archer
--     archer' = proc _ -> do
--       -- vx <- hold . stdNoiseR 2 (-10,10) 1 -< ()
--       -- vy <- hold . stdNoiseR 3 (-10,10) 2 -< ()
--       -- pos <- integral x0 -< V3 vx vy 0
--       (pos, vx, vy) <- returnA -< (x0,0,0)
--       returnA -< Archer (Body 1 pos) (atan2 vy vx)

-- archerH :: forall s t e m. (Monad m, Monoid e, HasTime t s, Fractional t) => V3D -> Wire s e m (Event Hit) Archer
-- archerH x0 = hittable archer' . arr ((),)
--   where
--     archer' :: Wire s e m () Archer
--     archer' = proc _ -> do
--       -- vx <- hold . stdNoiseR 2 (-10,10) 1 -< ()
--       -- vy <- hold . stdNoiseR 3 (-10,10) 2 -< ()
--       -- pos <- integral x0 -< V3 vx vy 0
--       (pos, vx, vy) <- returnA -< (x0,0,0)
--       returnA -< Archer (Body 1 pos) (atan2 vy vx)

-- hittable :: (Monad m, Monoid e) => Wire s e m a b -> Wire s e m (a, Event Hit) b
-- hittable wr = proc (a,h) -> do
--   x <- wr -< a
--   alive <- W.until -< (x, h)
--   returnA -< alive

testStage :: Wire (Timed Double ()) () IO () Stage -> IO ()
testStage w =
#ifdef WINDOWS
  runBackend
    (glutBackend (1/30) 5 (600,600) (50,50,50))
    (const . return $ ())
    (w . pure ())
#else
  runBackend
    (sdlBackend 600 600 (50,50,50))
    (const . return . return $ ())
    (w . pure ())
#endif




-- runTestSDL :: Wire (Timed Double ()) String IO (Event RenderEvent) [Planet] -> IO ()
-- runTestSDL w =
--   runBackend (sdlBackend 600 600 (31,31,31)) (const . return . return $ ()) (PlanetList <$> w)
