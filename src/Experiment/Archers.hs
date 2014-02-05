{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Category
import Control.Monad
import Data.Monoid as M
import Control.Wire.Unsafe.Event
import Control.Monad.Fix
import Control.Wire                     as W
import Data.Maybe                       (catMaybes)
import Experiment.Archers.Types
import Data.Traversable
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
    -- d0s <- evalRandIO . replicateM 20 $ (,) <$> ((^+^ (V3 (w/4) (h/4) 0)) . (^/ 2) <$> genPos) <*> genVel
    gen <- evalRandIO getRandom
    -- print a0s
    -- print d0s
    -- testStage (simpleStage0 w h a0s d0s)
    testStage (simpleStage1 w h a0s gen)
  where
    w = 400
    h = 300
    genPos :: RandomGen g => Rand g (V3 Double)
    genPos = V3 <$> getRandomR (0,w) <*> getRandomR (0,h) <*> pure 0
    -- genVel :: RandomGen g => Rand g (V3 Double)
    -- genVel = V3 <$> getRandomR (-20,20) <*> getRandomR (-20,20) <*> pure 0

simpleStage1 :: forall m e t s. (MonadFix m, Monoid e, HasTime t s, Fractional t)
    => Double
    -> Double
    -> [(V3D,Int)]
    -> Int
    -> Wire s e m () Stage
simpleStage1 w h a0s gen = proc _ -> do
  as <- zipArrow (map (uncurry archerWire) a0s) . pure [] -< ()
  newDarts <- popDart gen -< d0w
  ds <- krSwitch (pure []) -< (NoEvent, continuize <$> newDarts)
  -- dAs <- accumE (++) [] . popDart -< [d0w]
  -- ds <- rSwitch (pure []) -< (NoEvent,sequenceA <$> dAs)
  -- ds <- sequenceA dAs -<< NoEvent
  -- rec
  --   ds <- manageDarts -< ds
  returnA -< Stage w h (catMaybes as) (catMaybes ds)
  where
    continuize dWire dsWire = proc hitds -> do
      d <- dWire -< hitds
      ds <- dsWire -< hitds
      returnA -< d:ds
    -- manageDarts = proc ds -> do
    --   accumE [] -< 
    -- popDart = never . stdWackelkontakt 1 (1/2) gen <|> now
    -- popDart = (once . now . stdWackelkontakt 1 (1/10) gen) <|> never
    -- popDart = W.until . arr (\e -> (e,e)) . now . stdWackelkontakt 1 (1/2) gen <|> never
    -- popDart = proc d -> do
    --   popped <- now . stdWackelkontakt 1 (1/2) gen -< d
    --   now -< d
    -- popDart = proc d -> do
    --   popped <- became (> 5) . stdNoiseR 1 (0,10) gen -< ()
    --   returnA -< popped <$ d
    -- popDart = periodic 5
    popDart g = now . stdWackelkontakt 0.1 (1 - 1/50) g --> popDart (g+1)
    -- popDart g = proc dW -> do
    --   popped <- became (> 5) . stdNoiseR 1 (0,10) g -< ()
    --   now -< dW
    d0w :: Wire s e m (Event Archer) (Maybe Dart)
    d0w = dartWire (V3 (w/2) (h/2) 0) (V3 5 5 0)



simpleStage0 :: forall m t s e. (MonadFix m, HasTime t s, Monoid e, Fractional t)
    => Double           -- width
    -> Double           -- height
    -> [(V3D,Int)]      -- archers
    -> [(V3D,V3D)]      -- darts
    -> Wire s e m () Stage
simpleStage0 w h a0s d0s = proc _ -> do
    -- (as,ds) <- hitInteraction' -< ()
    rec
      let
        (hitas, hitds) = hitWatcher as ds
      as <- zipArrow (map (uncurry archerWire) a0s) -< hitas
      ds <- zipArrow (map (uncurry dartWire) d0s) -< hitds
    returnA -< Stage w h (catMaybes as) (catMaybes ds)
  where
    hitWatcher as ds = (hitas, hitds)
      where
        hitMatrix = map hitad as
        hitad a   = map (collision a) ds
        collision (Just a@(Archer (Body _ pa) _))
                  (Just d@(Dart (Body _ pd) _))
                  | norm (pa ^-^ pd) < 5  = Event (a,d)
                  | otherwise             = NoEvent
        collision _ _ = NoEvent
        hitas     = map ((snd <$>) . mergeEs) hitMatrix
        hitds     = map ((fst <$>) . mergeEs) (transpose hitMatrix)

mergeEs :: [Event a] -> Event a
mergeEs = foldl (merge const) NoEvent

chunks :: Int -> [a] -> [[a]]
chunks n xs = takeWhile (not . null) $ unfoldr (Just . splitAt n) xs

zipArrow :: (Monad m, Monoid e)
    => [Wire s e m (Event a) b]
    -> Wire s e m [Event a] [b]
zipArrow arrs = go arrs
  where
    go [] = pure []
    go (a:as) = proc hitas -> do
      case hitas of
        (h:hs) -> do
          a' <- a . delay NoEvent -< h
          as' <- go as -< hs
          returnA -< a':as'
        [] -> do
          a' <- a -< NoEvent
          as' <- sequenceA as -< NoEvent
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
