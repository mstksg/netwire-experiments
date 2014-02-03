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
import Control.Monad.Random
import Data.List (unfoldr, transpose)
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
main = do
    a0s <- evalRandIO . replicateM 20 $ (,) <$> genPos <*> getRandom
    d0s <- evalRandIO . replicateM 20 $ (,) <$> ((^+^ (V3 (w/4) (h/4) 0)) . (^/ 2) <$> genPos) <*> genVel
    print a0s
    print d0s
    testStage (simpleStage w h a0s d0s)
  where
    w = 400
    h = 300
    genPos = V3 <$> getRandomR (0,w) <*> getRandomR (0,h) <*> pure 0
    genVel = V3 <$> getRandomR (-20,20) <*> getRandomR (-20,20) <*> pure 0


simpleStage :: forall m t s e. (MonadFix m, HasTime t s, Monoid e, Fractional t)
    => Double           -- width
    -> Double           -- height
    -> [(V3D,Int)]      -- archers
    -> [(V3D,V3D)]      -- darts
    -> Wire s e m () Stage
simpleStage w h a0s d0s = proc _ -> do
    (as,ds) <- hitInteraction' -< ()
    returnA -< Stage w h as ds
  where
    aCount = length a0s
    dCount = length d0s

    hitInteraction' :: Wire s e m () ([Archer], [Dart])
    hitInteraction' = proc _ -> do
      rec
        let
          chunked = chunks dCount hits
          transposed = transpose chunked
          hitas = map (fmap snd . mergeEs) chunked
          hitds = map (fmap fst . mergeEs) transposed

        as <- zipArrow (map (uncurry archerWire) a0s) -< hitas
        ds <- zipArrow (map (uncurry dartWire) d0s) -< hitds

        hits <- zipHits hitWire aCount dCount -< (as, ds)

      returnA -< (catMaybes as, catMaybes ds)

mergeEs :: [Event a] -> Event a
mergeEs = foldl (merge const) NoEvent

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
    -> Int
    -> Wire s e m ([a1],[a2]) [b]
zipHits wr ac dc = go ((,) <$> [0..(ac-1)] <*> [0..(dc-1)])
  where
    go [] = pure []
    go ((ai,di):adis) = proc ads@(as,ds) -> do
      h <- wr -< (as !! ai, ds !! di)
      hs <- go adis -< ads
      returnA -< h:hs

data Hit = Hit

archerWire :: (Monad m, Monoid e, HasTime t s)
    => V3D
    -> Int
    -> Wire s e m (Event Dart) (Maybe Archer)
archerWire x0 gen = proc h -> do
  (vx,vy) <- hold . stdNoiseR 2 ((-10,-10),(10,10)) gen -< ()
  xa <- integral x0 -< V3 vx vy 0
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

hitWire :: (Monad m, Monoid e) => Wire s e m (Maybe Archer, Maybe Dart) (Event (Archer, Dart))
hitWire = proc ad ->
  case ad of
    (Just a@(Archer (Body _ xa) _), Just d@(Dart (Body _ xd) _)) ->
      never . W.unless ((< 5) . norm) . arr fst --> now . arr snd
        -< (xa ^-^ xd, (a,d))
    _ ->
      never
        -< ()


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
