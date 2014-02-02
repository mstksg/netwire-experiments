{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Category
import Control.Monad
import Control.Wire.Unsafe.Event
import Control.Monad.Fix
import Control.Wire                     as W
import Data.Maybe                       (catMaybes)
import Experiment.Archers.Types
import FRP.Netwire
import Data.List (unfoldr)
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
    (as,ds) <- hitInteraction' -< ()
    returnA -< Stage w h as ds
  where
    -- hitInteraction :: Wire s e m () ([Archer], [Dart])
    -- hitInteraction = proc _ -> do
    --   rec
    --     let
    --       hita1 = fmap snd . mergeEs $ [ hit11, hit12, hit13 ]
    --       hita2 = fmap snd . mergeEs $ [ hit21, hit22, hit23 ]
    --       hita3 = fmap snd . mergeEs $ [ hit31, hit32, hit33 ]
    --       hitd1 = fmap fst . mergeEs $ [ hit11, hit21, hit21 ]
    --       hitd2 = fmap fst . mergeEs $ [ hit12, hit22, hit32 ]
    --       hitd3 = fmap fst . mergeEs $ [ hit13, hit23, hit33 ]
    --     a1 <- archerWire a10 . delay NoEvent -< hita1
    --     a2 <- archerWire a20 . delay NoEvent -< hita2
    --     a3 <- archerWire a30 . delay NoEvent -< hita3
    --     d1 <- dartWire x10 v10 . delay NoEvent -< hitd1
    --     d2 <- dartWire x20 v20 . delay NoEvent -< hitd2
    --     d3 <- dartWire x30 v30 . delay NoEvent -< hitd3
    --     hit11 <- hitWire -< (a1,d1)
    --     hit12 <- hitWire -< (a1,d2)
    --     hit13 <- hitWire -< (a1,d3)
    --     hit21 <- hitWire -< (a2,d1)
    --     hit22 <- hitWire -< (a2,d2)
    --     hit23 <- hitWire -< (a2,d3)
    --     hit31 <- hitWire -< (a3,d1)
    --     hit32 <- hitWire -< (a3,d2)
    --     hit33 <- hitWire -< (a3,d3)
    --   returnA -< (catMaybes [a1,a2,a3], catMaybes [d1,d2,d3])

    hitInteraction' :: Wire s e m () ([Archer], [Dart])
    hitInteraction' = proc _ -> do
      rec
        let
          hitas = map (fmap snd . mergeEs) $ chunks 3 hits
          hitds = map (fmap fst . mergeEs) $ everys 3 hits

        as <- zipArrow (map archerWire [a10,a20,a30]) -< hitas
        ds <- zipArrow (map (uncurry dartWire) [(x10,v10),(x20,v20),(x30,v30)]) -< hitds


        hits <- zipHits hitWire 3 -< (as, ds)

        -- a1 <- archerWire a10 . delay NoEvent . arr (!! 1) -< [hita1, hita2, hita3]
        -- a2 <- archerWire a20 . delay NoEvent . arr (!! 2) -< [hita1, hita2, hita3]
        -- a3 <- archerWire a30 . delay NoEvent . arr (!! 3) -< [hita1, hita2, hita3]



        -- [a1,a2,a3] <- undefined -< [hita1, hita2, hita3]


        -- hit11 <- hitWire . arr (\(as,ds) -> (as !! 0, ds !! 0)) -< (as,ds)
        -- hit12 <- hitWire . arr (\(as,ds) -> (as !! 0, ds !! 1)) -< (as,ds)
        -- hit13 <- hitWire . arr (\(as,ds) -> (as !! 0, ds !! 2)) -< (as,ds)
        -- hit21 <- hitWire . arr (\(as,ds) -> (as !! 1, ds !! 0)) -< (as,ds)
        -- hit22 <- hitWire . arr (\(as,ds) -> (as !! 1, ds !! 1)) -< (as,ds)
        -- hit23 <- hitWire . arr (\(as,ds) -> (as !! 1, ds !! 2)) -< (as,ds)
        -- hit31 <- hitWire . arr (\(as,ds) -> (as !! 2, ds !! 0)) -< (as,ds)
        -- hit32 <- hitWire . arr (\(as,ds) -> (as !! 2, ds !! 1)) -< (as,ds)
        -- hit33 <- hitWire . arr (\(as,ds) -> (as !! 2, ds !! 2)) -< (as,ds)

        -- a1 <- archerWire a10 . delay NoEvent -< hita1
        -- a2 <- archerWire a20 . delay NoEvent -< hita2
        -- a3 <- archerWire a30 . delay NoEvent -< hita3
        -- d1 <- dartWire x10 v10 . delay NoEvent -< hitd1
        -- d2 <- dartWire x20 v20 . delay NoEvent -< hitd2
        -- d3 <- dartWire x30 v30 . delay NoEvent -< hitd3
        -- hit11 <- hitWire -< (a1,d1)
        -- hit12 <- hitWire -< (a1,d2)
        -- hit13 <- hitWire -< (a1,d3)
        -- hit21 <- hitWire -< (a2,d1)
        -- hit22 <- hitWire -< (a2,d2)
        -- hit23 <- hitWire -< (a2,d3)
        -- hit31 <- hitWire -< (a3,d1)
        -- hit32 <- hitWire -< (a3,d2)
        -- hit33 <- hitWire -< (a3,d3)

      returnA -< (catMaybes as, catMaybes ds)

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

everys :: Int -> [a] -> [[a]]
everys n xs = map (\i -> every n (drop i xs)) [0..(n-1)]

every :: Int -> [a] -> [a]
every _ []   = []
every n (x:xs) = x : every n (drop (n-1) xs)
-- every n xs = case drop (n-1) xs of
--               (y:ys) -> y : every n ys
--               [] -> []

chunks :: Int -> [a] -> [[a]]
chunks n xs = takeWhile (not . null) $ unfoldr (Just . splitAt n) xs

zipArrow :: Monad m
    => [Wire s e m (Event a) b]
    -> Wire s e m [Event a] [b]
zipArrow arrs = go arrs 0
  where
    go [] _ = pure []
    go (a:as) n = proc hitas -> do
      a' <- a . delay NoEvent -< hitas !! n
      as' <- go as (n+1) -< hitas
      returnA -< a':as'

zipHits :: Monad m
    => Wire s e m (a1,a2) b
    -> Int
    -> Wire s e m ([a1],[a2]) [b]
zipHits wr n = go ((,) <$> [0..(n-1)] <*> [0..(n-1)])
  where
    go [] = pure []
    go ((ai,di):adis) = proc ads@(as,ds) -> do
      h <- wr -< (as !! ai, ds !! di)
      hs <- go adis -< ads
      returnA -< h:hs


data Hit = Hit

archerWire :: (Monad m, Monoid e)
    => V3D
    -> Wire s e m (Event Dart) (Maybe Archer)
archerWire x0 = proc h -> do
  xa <- pure x0 -< ()
  W.until --> pure Nothing
    -< (Just (Archer (Body 1 xa) 0), h)

dartWire :: (Monad m, Monoid e, HasTime t s)
    => V3D
    -> V3D
    -> Wire s e m (Event Archer) (Maybe Dart)
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


hitWire :: (Monad m, Monoid e) => Wire s e m (Maybe Archer, Maybe Dart) (Event (Archer, Dart))
hitWire = proc ad ->
  case ad of
    (Just a@(Archer (Body _ xa) _), Just d@(Dart (Body _ xd) _)) ->
      never . W.unless ((< 5) . norm) . arr fst --> now . arr snd
        -< (xa ^-^ xd, (a,d))
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
