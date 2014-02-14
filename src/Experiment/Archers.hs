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
import Data.Maybe                (isNothing)
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
import Utils.Wire.Wrapped

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
      hitas' = map (() <$) hitas
      hitds' = map (() <$) hitds
      -- as = length asnds `traceShow` map fst asnds
      as = map fst asnds
      newDarts = mconcat (map snd asnds)
      -- newDartWires :: Event [Wire s e m () Dart]
      newDartWires = map (uncurry dartWire) <$> newDarts
      -- starta0s :: Event [Wire s e m ([Archer],Event ()) (Archer, Event [(V3D,V3D)])]
    -- asds <- zipArrow (map (uncurry archerWire) a0s) . delay mempty -< zip hitas (unDupSelf as)
    -- asds <- zipArrow (map (uncurry archerWire) a0s) -< unDupSelf as
    starta0s <- now -< map (uncurry archerWire) a0s
    -- asnds <- wireBox [] -< ((starta0s,hitas'),unDupSelf as)
    asnds <- dWireBox' ([], NoEvent) -< (starta0s,zip (unDupSelf as) hitas')
    -- asnds <- wireBox (map (uncurry archerWire) a0s) . delay ((NoEvent,repeat NoEvent),repeat []) -< ((NoEvent,hitas'),unDupSelf as)
    ds    <- dWireBox' NoEvent -< (newDartWires,hitds')
  returnA -< Stage w h as ds
  where
  hitWatcher' :: [Archer] -> [Dart] -> ([Event Messages],[Event Messages])
  hitWatcher' as ds = (hitas, hitds)
    where
      hitMatrix = map hitad as
      hitad a   = map (collision a) ds
      collision (Archer (Body _ pa) _)
                (Dart (Body _ pd) _)
                | norm (pa ^-^ pd) < 5  = Event [Die]
                | otherwise             = NoEvent
      -- collision _ _ = NoEvent
      hitas     = map mergeEs hitMatrix
      hitds     = map mergeEs (transpose hitMatrix)




-- simpleStage2 :: forall m e t s. (MonadFix m, Monoid e, HasTime t s, Fractional t)
--     => Double
--     -> Double
--     -> [(V3D,Int)]
--     -> Int
--     -> Wire s e m () Stage
-- simpleStage2 w h a0s gen = proc _ -> do
--   rec
--     let
--       as = map fst asds
--       newDartEvents = map snd asds
--       newDarts :: Event [(V3D,V3D)]
--       newDarts = mconcat newDartEvents
--       dartWirer :: (V3D, V3D) -> Wire s e m (Event Messages) (Maybe Dart)
--       dartWirer = uncurry dartWire
--       newDartWires = map dartWirer <$> newDarts
--       (hitas, hitds) = hitWatcher as ds
--       dieds = borderWatcher ds
--       dmess = zipWith (M.<>) hitds dieds
--     asds <- zipArrow (map (uncurry archerWire) a0s) . delay mempty -< zip hitas (unDupSelf as)
--     -- newDarts <- popDart gen -< d0w
--     ds <- krSwitch (pure []) . delay ([],NoEvent) -< (dmess, continuize <$> newDartWires)
--     -- ds <- undefined -< undefined

  -- returnA -< Stage w h (catMaybes as) (catMaybes ds)
  -- where
  --   continuize ::
  --          [Wire s e m (Event Messages) (Maybe Dart)]
  --       -> Wire s e m [Event Messages] [Maybe Dart]
  --       -> Wire s e m [Event Messages] [Maybe Dart]
  --   continuize [] oldWires = proc mess -> do
  --     ds <- oldWires -< mess
  --     returnA -< ds
  --   continuize (dw:dws) oldWires = proc mess -> do
  --     case mess of
  --       (m:ms) -> do
  --         d <- dw -< m
  --         ds <- continuize dws oldWires -< ms
  --         returnA -< d:ds
  --       [] -> do
  --         d <- dw . never -< []
  --         ds <- continuize dws oldWires . arr (:[]) . never -< []
  --         returnA -< d:ds
  --   -- continuize dWire dsWire = proc hitds -> do
  --   --   case hitds of
  --   --     (hit:hits) -> do
  --   --       d <- dWire -< hit
  --   --       ds <- dsWire -< hits
  --   --       returnA -< d:ds
  --   --     [] -> do
  --   --       d <- dWire . never -< []
  --   --       ds <- dsWire . arr (:[]) . never -< []
  --   --       returnA -< d:ds
  --   -- popDart g = now . stdWackelkontakt 0.1 (1 - 1/75) g --> popDart (g+1)
  --   borderWatcher :: [Maybe Dart] -> [Event Messages]
  --   borderWatcher = map outOfBounds
  --     where
  --       outOfBounds (Just (Dart (Body _ (V3 x y _)) _)) =
  --         if or [x < 0, y < 0, x > w, y > h]
  --           then Event [Die]
  --           else NoEvent
  --       outOfBounds _ = NoEvent
  --   d0w :: Wire s e m (Event Messages) (Maybe Dart)
  --   d0w = dartWire (V3 (w/2) (h/2) 0) (V3 10 10 0)

type DartData = (V3D,V3D)

-- manageDarts :: Monad m => Wire s e m (Event [DartData], [Event Messages]) [Maybe Dart]
-- manageDarts = proc (dds, hitds) -> do
--   undefined -< undefined


-- simpleStage1 :: forall m e t s. (MonadFix m, Monoid e, HasTime t s, Fractional t)
--     => Double
--     -> Double
--     -> [(V3D,Int)]
--     -> Int
--     -> Wire s e m () Stage
-- simpleStage1 w h a0s gen = proc _ -> do
--   rec
--     let
--       as = map fst asds
--       newDartEvents = map snd asds
--       newDarts :: Event [(V3D,V3D)]
--       newDarts = mconcat newDartEvents
--       dartWirer :: (V3D, V3D) -> Wire s e m (Event Messages) (Maybe Dart)
--       dartWirer = uncurry dartWire
--       newDartWires = map dartWirer <$> newDarts
--       (hitas, hitds) = hitWatcher as ds
--       dieds = borderWatcher ds
--       dmess = zipWith (M.<>) hitds dieds
--     asds <- zipArrow (map (uncurry archerWire) a0s) . delay mempty -< zip hitas (unDupSelf as)
--     -- newDarts <- popDart gen -< d0w
--     ds <- krSwitch (pure []) . delay ([],NoEvent) -< (dmess, continuize <$> newDartWires)
--     -- ds <- undefined -< undefined

  -- returnA -< Stage w h (catMaybes as) (catMaybes ds)
  -- where
  --   continuize ::
  --          [Wire s e m (Event Messages) (Maybe Dart)]
  --       -> Wire s e m [Event Messages] [Maybe Dart]
  --       -> Wire s e m [Event Messages] [Maybe Dart]
  --   continuize [] oldWires = proc hitds -> do
  --     ds <- oldWires -< hitds
  --     returnA -< ds
  --   continuize (dw:dws) oldWires = proc mess -> do
  --     case mess of
  --       (m:ms) -> do
  --         d <- dw -< m
  --         ds <- continuize dws oldWires -< ms
  --         returnA -< d:ds
  --       [] -> do
  --         d <- dw . never -< []
  --         ds <- continuize dws oldWires . arr (:[]) . never -< []
  --         returnA -< d:ds
  --   -- continuize dWire dsWire = proc hitds -> do
  --   --   case hitds of
  --   --     (hit:hits) -> do
  --   --       d <- dWire -< hit
  --   --       ds <- dsWire -< hits
  --   --       returnA -< d:ds
  --   --     [] -> do
  --   --       d <- dWire . never -< []
  --   --       ds <- dsWire . arr (:[]) . never -< []
  --   --       returnA -< d:ds
  --   popDart g = now . stdWackelkontakt 0.1 (1 - 1/75) g --> popDart (g+1)
  --   borderWatcher :: [Maybe Dart] -> [Event Messages]
  --   borderWatcher = map outOfBounds
  --     where
  --       outOfBounds (Just (Dart (Body _ (V3 x y _)) _)) =
  --         if or [x < 0, y < 0, x > w, y > h]
  --           then Event [Die]
  --           else NoEvent
  --       outOfBounds _ = NoEvent
  --   d0w :: Wire s e m (Event Messages) (Maybe Dart)
  --   d0w = dartWire (V3 (w/2) (h/2) 0) (V3 10 10 0)

hitWatcher :: [Maybe Archer] -> [Maybe Dart] -> ([Event Messages],[Event Messages])
hitWatcher as ds = (hitas, hitds)
  where
    hitMatrix = map hitad as
    hitad a   = map (collision a) ds
    collision (Just (Archer (Body _ pa) _))
              (Just (Dart (Body _ pd) _))
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

-- simpleStage0 :: forall m t s e. (MonadFix m, HasTime t s, Monoid e, Fractional t)
--     => Double           -- width
--     -> Double           -- height
--     -> [(V3D,Int)]      -- archers
--     -> [(V3D,V3D)]      -- darts
--     -> Wire s e m () Stage
-- simpleStage0 w h a0s d0s = proc _ -> do
--     -- (as,ds) <- hitInteraction' -< ()
--     rec
--       let
--         (hitas, hitds) = hitWatcher as ds
--       as <- zipArrow (map (uncurry archerWire) a0s) -< hitas
--       ds <- zipArrow (map (uncurry dartWire) d0s) -< hitds
--     returnA -< Stage w h (catMaybes as) (catMaybes ds)
--   where
--     -- hitWatcher as ds = (hitas, hitds)
--     --   where
--     --     hitMatrix = map hitad as
--     --     hitad a   = map (collision a) ds
--     --     collision (Just a@(Archer (Body _ pa) _))
--     --               (Just d@(Dart (Body _ pd) _))
--     --               | norm (pa ^-^ pd) < 5  = Event (a,d)
--     --               | otherwise             = NoEvent
--     --     collision _ _ = NoEvent
--     --     hitas     = map ((snd <$>) . mergeEs) hitMatrix
--     --     hitds     = map ((fst <$>) . mergeEs) (transpose hitMatrix)

mergeEs :: [Event a] -> Event a
mergeEs = foldl (merge const) NoEvent

chunks :: Int -> [a] -> [[a]]
chunks n xs = takeWhile (not . null) $ unfoldr (Just . splitAt n) xs

-- zipArrow' :: (Monad m, Monoid e)
--     => [Wire s e m (Event a, [c]) b]
--     -> Wire s e m ([Event a],[c]) [b]
-- zipArrow' arrs = go arrs
--   where
--     go [] = pure []
--     go (a:as) = proc (hitas,x) -> do
--       case hitas of
--         (h:hs) -> do
--           a' <- a . delay (NoEvent,[]) -< (h,x)
--           as' <- go as -< (hs,x)
--           returnA -< a':as'
--         [] -> do
--           a' <- a -< (NoEvent,x)
--           as' <- sequenceA as -< (NoEvent,x)
--           returnA -< a':as'


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
    -> Wire s e m ([Archer], Event ()) (Archer, Event [(V3D,V3D)])
archerWire x0 _ = shootCycle . seek
  where
    range = 30
    speed = 10
    dartSpeed = 20
    coolDownTime = 3
    seek :: Wire s e m ([Archer], Event()) (Archer, Maybe V3D)
    seek = proc (others,die) -> do
      rec
        let
          otherPs :: [(Double,V3D)]
          otherPs = map ( (fst &&& uncurry (flip (^/)))
                        . (norm &&& id)
                        . (^-^ pos)
                        . bodyPos
                        . archerBody
                        ) others
          target | null otherPs = Nothing
                 | otherwise    = Just $ minimumBy (comparing fst) otherPs
          (vel,target') =
            case target of
              Just (dist,targDir)
                | dist > range -> (targDir ^* speed, Nothing)
                -- | dist < 1     -> error $ "what is going on " ++ show dist
                | otherwise    -> (zero, Just targDir)
              _                -> (zero, Nothing)
          angle = 0
        pos <- integral x0 -< vel
      W.until -< ((Archer (Body 1 pos) angle, target'), die)

    shootCycle :: Wire s e m (Archer, Maybe V3D) (Archer, Event [(V3D,V3D)])
    shootCycle = waiting --> shoot --> shootCycle
      where
        waiting :: Wire s e m (Archer, Maybe V3D) (Archer, Event [(V3D,V3D)])
        waiting = second never . W.when (isNothing . snd)
        shoot :: Wire s e m (Archer, Maybe V3D) (Archer, Event [(V3D,V3D)])
        shoot = proc (self, targetDir) -> do
          case (self, targetDir) of
            (Archer (Body _ p) 0, Just tDir) -> do
              let
                dartData = [(p ^+^ (tDir ^* 8), tDir ^* dartSpeed)]
              shot <- now -< dartData
              returnA . W.for coolDownTime -< (self, shot)
            _ -> do
              inhibit mempty -< ()


    -- dead :: Wire s e m (Event Messages, [Maybe Archer]) (Maybe Archer, Event [(V3D,V3D)])
    -- dead = pure (Nothing, NoEvent)

-- proc (mess,as) -> do
  -- die <- filterE (any isDie) -< mess
  -- (vx,vy) <- hold . stdNoiseR 2 ((-10,-10),(10,10)) gen -< ()
  -- xa <- integral x0 -< V3 vx vy 0
  -- W.until --> pure (Nothing, NoEvent)
  --   -< ((Just (Archer (Body 1 xa) 0), NoEvent), die)
  -- where
  --   d0 = (V3 0 0 0, V3 10 10 0)

-- archerWire :: (Monad m, Monoid e, HasTime t s)
--     => V3D
--     -> Int
--     -> Wire s e m (Event Messages, [Maybe Archer]) (Maybe Archer, Event (V3D,V3D))
-- archerWire x0 gen = proc (mess,as) -> do
--   die <- filterE (any isDie) -< mess
--   (vx,vy) <- hold . stdNoiseR 2 ((-10,-10),(10,10)) gen -< ()
--   xa <- integral x0 -< V3 vx vy 0
--   W.until --> pure (Nothing, NoEvent)
--     -< ((Just (Archer (Body 1 xa) 0), NoEvent), die)
--   where
--     d0 = (V3 0 0 0, V3 10 10 0)

dartWire :: forall m e t s. (Monad m, Monoid e, HasTime t s)
    => V3D
    -> V3D
    -> Wire s e m (Event ()) Dart
dartWire x0 v0@(V3 vx vy _) = proc die -> do
  -- die <- filterE (any isDie) -< mess
  pos <- integral x0 -< v0
  W.until -< (Dart (Body 1 pos) (atan2 vy vx), die)
  -- W.until --> pure Nothing
  --   -< (Just (Dart (Body 1 pos) (atan2 vy vx)), die)

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
