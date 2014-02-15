{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Main where

-- import Data.Monoid            as M
-- import Debug.Trace
import Control.Category
import Control.Monad as M hiding (sequence)
import Control.Monad.Fix
import Control.Monad.Random
import Control.Wire              as W
import Control.Wire.Unsafe.Event
import Data.List                 (unfoldr, transpose, minimumBy)
import Data.Maybe                (mapMaybe)
import Data.Ord                  (comparing)
import Data.Traversable
import Experiment.Archers.Types
import FRP.Netwire
import Linear.Metric
import Linear.V3
import Linear.Vector
import Physics
import Prelude hiding            ((.),id,sequence)
import Render.Render
import Utils.Wire.Misc
import Utils.Wire.Noise
import Utils.Wire.Wrapped

#ifdef WINDOWS
import Render.Backend.GLUT
import Experiment.Archers.Instances.GLUT    ()
#else
import Render.Backend.SDL
import Experiment.Archers.Instances.SDL ()
#endif



main :: IO ()
main = do
    a0s <- evalRandIO . replicateM 13 $ (,) <$> genPos <*> getRandom
    -- d0s <- evalRandIO . replicateM 20 $ (,) <$> ((^+^ (V3 (w/4) (h/4) 0)) . (^/ 2) <$> genPos) <*> genVel
    gen <- evalRandIO getRandom
    print a0s
    -- print d0s
    -- testStage (simpleStage0 w h a0s d0s)
    testStage (simpleStage3 w h a0s gen)
  where
    w = 400
    h = 300
    genPos :: RandomGen g => Rand g (V3 Double)
    genPos = V3 <$> getRandomR (0,w) <*> getRandomR (0,h) <*> pure 0
    -- genVel :: RandomGen g => Rand g (V3 Double)
    -- genVel = V3 <$> getRandomR (-20,20) <*> getRandomR (-20,20) <*> pure 0


simpleStage3 :: forall m e t s. (MonadFix m, Monoid e, HasTime t s, Fractional t)
  => Double
  -> Double
  -> [(V3D,Int)]
  -> Int
  -> Wire s e m () Stage
simpleStage3 w h a0s _ = proc _ -> do
  rec
    let
      (hitas,hitds) = hitWatcher' as ds
      hitds' = map (() <$) hitds
      outds  = map (() <$) (borderWatcher ds)
      allds  = zipWith (<>) outds hitds'
      as = map fst asnds
      newDarts = mconcat (map snd asnds)
      newDartWires = map (uncurry . uncurry $ dartWire) <$> newDarts
    starta0s <- now -< map (uncurry archerWire) a0s
    asnds <- dWireBox' ([], NoEvent) -< (starta0s,zip (unDupSelf as) hitas)
    ds    <- dWireBox' NoEvent -< (newDartWires,allds)
  returnA -< Stage w h as ds
  where
  hitWatcher' :: [Archer] -> [Dart] -> ([Event Messages],[Event Messages])
  hitWatcher' as ds = (hitas, hitds)
    where
      hitMatrix = map hitad as
      hitad a   = map (collision a) ds
      collision (Archer pa _ _)
                (Dart pd dd _)
                | norm (pa ^-^ pd) < 5  = Event [Hit dd]
                | otherwise             = NoEvent
      -- collision _ _ = NoEvent
      hitas     = map mergeEs hitMatrix
      hitds     = map mergeEs (transpose hitMatrix)
  borderWatcher :: [Dart] -> [Event Messages]
  borderWatcher = map outOfBounds
    where
      outOfBounds (Dart (V3 x y _) _ _)
        | or [x < 0, y < 0, x > w, y > h] = Event [Die]
        | otherwise                       = NoEvent




type DartData = (V3D,V3D)

hitWatcher :: [Maybe Archer] -> [Maybe Dart] -> ([Event Messages],[Event Messages])
hitWatcher as ds = (hitas, hitds)
  where
    hitMatrix = map hitad as
    hitad a   = map (collision a) ds
    collision (Just (Archer pa  _ _))
              (Just (Dart pd _ _))
              | norm (pa ^-^ pd) < 5  = Event [Die]
              | otherwise             = NoEvent
    collision _ _ = NoEvent
    hitas     = map mergeEs hitMatrix
    hitds     = map mergeEs (transpose hitMatrix)

unDupSelf :: [a] -> [[a]]
unDupSelf = go []
  where
    go _ [] = []
    go xs (y:ys) = (xs++ys):go (y:xs) ys

mergeEs :: [Event a] -> Event a
mergeEs = foldl (merge const) NoEvent

chunks :: Int -> [a] -> [[a]]
chunks n xs = takeWhile (not . null) $ unfoldr (Just . splitAt n) xs

zipArrow :: (Monad m, Monoid e, Monoid a)
    => [Wire s e m a b]
    -> Wire s e m [a] [b]
zipArrow arrs = go arrs
  where
    go [] = pure []
    go (a:as) = proc inputs -> do
      case inputs of
        (x:xs) -> do
          a' <- a -< x
          as' <- go as -< xs
          returnA -< a':as'
        [] -> do
          a' <- a -< mempty
          as' <- sequenceA as -< mempty
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

archerWire :: forall m e t s. (MonadFix m, Monoid e, HasTime t s)
    => V3D
    -> Int
    -> Wire s e m ([Archer], Event Messages) (Archer, Event [((V3D,V3D),Double)])
archerWire x0 gen = proc (others,mess) -> do
    rec
      let
        target = seek others pos
        newD   = target >>= newDart pos
        vel    = case target of
          Just (tDist, tDir)
            | tDist > range  -> tDir ^* speed
          _                  -> 0
        hit =
          getSum . mconcat . fmap Sum . mapMaybe maybeHit <$> mess
      pos <- integral x0 -< vel
    shot <- shoot -< newD
    shotR <- couple (noisePrimR (0.67,1.5) gen') -< shot
    let shot' = (\(ds,r) -> map (,dartDmg / r) ds) <$> shotR
    health <- hold . accumE (-) startingHealth <|> pure startingHealth -< hit
    let
      angle = 0
      a = Archer pos (health / startingHealth) angle
    W.when (> 0) -< health
    returnA -< (a,shot')
  where
    gen' = mkStdGen gen
    newDart p (tDist, tDir)
      | tDist > range = Nothing
      | otherwise     = Just $ (p ^+^ tDir ^* 8, tDir ^* dartSpeed)
    range = 50
    startingHealth = 10
    speed = 7.5
    -- recovery = 0.1
    dartDmg = 2
    dartSpeed = 30
    coolDownTime = 4
    seek others pos | null otherPs = Nothing
                    | otherwise    = Just $ minimumBy (comparing fst) otherPs
      where
        otherPs = map ( (fst &&& uncurry (flip (^/)))
                      . (norm &&& id)
                      . (^-^ pos)
                      . archerPos
                      ) others
    shoot = (proc newD -> do
      case newD of
        Nothing -> never -< ()
        Just d  -> do
          shot <- W.for coolDownTime . now -< [d]
          returnA -< shot
      ) --> shoot




dartWire :: forall m e t s. (Monad m, Monoid e, HasTime t s)
    => V3D
    -> V3D
    -> Double
    -> Wire s e m (Event ()) Dart
dartWire x0 v0@(V3 vx vy _) damage = proc die -> do
  pos <- integral x0 -< v0
  W.until -< (Dart pos damage (atan2 vy vx), die)

testStage :: Wire (Timed Double ()) () Identity () Stage -> IO ()
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
