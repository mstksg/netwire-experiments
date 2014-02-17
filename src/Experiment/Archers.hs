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
import Data.Colour.Names
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
    t1a0s <- evalRandIO . replicateM 13 $ (,) <$> genPos <*> (mkStdGen <$> getRandom)
    t2a0s <- evalRandIO . replicateM 13 $ (,) <$> genPos <*> (mkStdGen <$> getRandom)
    -- d0s <- evalRandIO . replicateM 20 $ (,) <$> ((^+^ (V3 (w/4) (h/4) 0)) . (^/ 2) <$> genPos) <*> genVel
    -- gen <- evalRandIO getRandom
    -- print a0s
    -- print d0s
    -- testStage (simpleStage0 w h a0s d0s)
    -- testStage (simpleStage3 w h a0s gen)
    testStage (simpleStage4 w h (t1flag,t1a0s) (t2flag,t2a0s))
  where
    t1flag = TeamFlag red
    t2flag = TeamFlag blue
    w = 600
    h = 450
    genPos :: RandomGen g => Rand g (V3 Double)
    genPos = V3 <$> getRandomR (0,w) <*> getRandomR (0,h) <*> pure 0
    -- genVel :: RandomGen g => Rand g (V3 Double)
    -- genVel = V3 <$> getRandomR (-20,20) <*> getRandomR (-20,20) <*> pure 0

simpleStage4 ::
     Double
  -> Double
  -> (TeamFlag, [(V3D,StdGen)])
  -> (TeamFlag, [(V3D,StdGen)])
  -> Wire' () Stage
simpleStage4 w h (t1flag,t1a0s) (t2flag,t2a0s) = proc _ -> do
    rec
      let
        (t1ahits,t2dhits) = hitWatcher t1as t2ds
        (t2ahits,t1dhits) = hitWatcher t2as t1ds
        t1douts           = borderWatcher t1ds
        t2douts           = borderWatcher t2ds
        t1devts           = zipWith (<>) t1douts t1dhits
        t2devts           = zipWith (<>) t2douts t2dhits
      team1@(Team _ t1as t1ds) <- teamWire t1flag t1a0s -< (team2, (t1ahits,t1devts))
      team2@(Team _ t2as t2ds) <- teamWire t2flag t2a0s -< (team1, (t2ahits,t2devts))
    returnA -< Stage w h (t1as ++ t2as) (t1ds ++ t2ds)
  where
    hitWatcher :: [Archer] -> [Dart] -> ([Event Messages],[Event ()])
    hitWatcher as ds = (hitas, hitds)
      where
        hitMatrix = map hitad as
        hitad a   = map (collision a) ds
        collision (Archer pa _ _ _)
                  (Dart pd dd _)
                  | norm (pa ^-^ pd) < 5  = Event [Hit dd]
                  | otherwise             = NoEvent
        hitas     = map mergeEs hitMatrix
        hitds     = map ((() <$) . mergeEs) (transpose hitMatrix)
    borderWatcher :: [Dart] -> [Event ()]
    borderWatcher = map outOfBounds
      where
        outOfBounds (Dart (V3 x y _) _ _)
          | or [x < 0, y < 0, x > w, y > h] = Event ()
          | otherwise                       = NoEvent


teamWire :: (MonadFix m, Monoid e, HasTime t s)
  => TeamFlag
  -> [(V3D,StdGen)]
  -> Wire s e m (Team, ([Event Messages],[Event ()])) Team
teamWire f a0s = proc (Team _ others _, (messAs,messDs)) -> do
    starta0s <- now -< map archerWire aDatas
    asnds <- dWireBox' ([], NoEvent) -< (starta0s,zip (repeat others) messAs)
    let
      newDarts = mconcat (map snd asnds)
      newDartWires = map (uncurry . uncurry $ dartWire) <$> newDarts
      as = map fst asnds
    ds    <- dWireBox' NoEvent -< (newDartWires,messDs)
    returnA -< Team f as ds
  where
    aDatas = map (\(x0,gen) -> ArcherData x0 gen (Just f)) a0s

simpleStage3 ::
     Double
  -> Double
  -> [(V3D,StdGen)]
  -> Int
  -> Wire' () Stage
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
    starta0s <- now -< map archerWire aDatas
    asnds <- dWireBox' ([], NoEvent) -< (starta0s,zip (unDupSelf as) hitas)
    ds    <- dWireBox' NoEvent -< (newDartWires,allds)
  returnA -< Stage w h as ds
  where
    aDatas = map (\(x0,gen) -> ArcherData x0 gen Nothing) a0s
    hitWatcher' :: [Archer] -> [Dart] -> ([Event Messages],[Event Messages])
    hitWatcher' as ds = (hitas, hitds)
      where
        hitMatrix = map hitad as
        hitad a   = map (collision a) ds
        collision (Archer pa _ _ _)
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




-- hitWatcher :: [Maybe Archer] -> [Maybe Dart] -> ([Event Messages],[Event Messages])
-- hitWatcher as ds = (hitas, hitds)
--   where
--     hitMatrix = map hitad as
--     hitad a   = map (collision a) ds
--     collision (Just (Archer pa _ _ _))
--               (Just (Dart pd _ _))
--               | norm (pa ^-^ pd) < 5  = Event [Die]
--               | otherwise             = NoEvent
--     collision _ _ = NoEvent
--     hitas     = map mergeEs hitMatrix
--     hitds     = map mergeEs (transpose hitMatrix)

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
    => ArcherData
    -> Wire s e m ([Archer], Event Messages) (Archer, Event [DartData])
archerWire (ArcherData x0 gen flag) = proc (others,mess) -> do
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
    shotR <- couple (noisePrimR (0.67,1.5) gen) -< shot
    let shot' = (\(ds,r) -> map (,dartDmg / r) ds) <$> shotR
    damage <- hold . accumE (+) 0 <|> pure 0 -< hit
    rec
      let health = min (startingHealth + recov - damage) startingHealth
      recov <- integral 0 . ((pure recovery . W.when (< startingHealth)) <|> pure 0) . delay startingHealth -< health
    let
      angle = 0
      a = Archer pos (health / startingHealth) angle flag
    W.when (> 0) -< health
    returnA -< (a,shot')
  where
    newDart p (tDist, tDir)
      | tDist > range = Nothing
      | otherwise     = Just $ (p ^+^ tDir ^* 8, tDir ^* dartSpeed)
    range = 50
    startingHealth = 10
    speed = 7.5
    recovery = 0.1
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

testStage :: Wire' () Stage -> IO ()
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
