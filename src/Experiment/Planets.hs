{-# LANGUAGE CPP #-}

module Main where

import Control.Category
import Control.Monad.Writer.Strict
import Control.Wire                         as W
import Data.Char                            (ord)
import Data.Maybe                           (fromJust, isJust)
import Data.Traversable
import Experiment.Planets.Instances.GNUPlot ()
import Experiment.Planets.Types
import Linear.V3
import Linear.Vector
import Physics
-- import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Prelude hiding                       ((.), id)
import Render.Backend.GNUPlot
import Render.Render

#ifdef WINDOWS
import Render.Backend.GLUT
import Experiment.Planets.Instances.GLUT    ()
#else
import Render.Backend.SDL
import Experiment.Planets.Instances.SDL     ()
#endif

-- | What is this number?  Well, we want our graviational constant to be 1,
-- so we normalize with our time unit being a day and our distance unit
-- being an AU.  Our mass unit then must be 6.720074483812448e33 kg.  To
-- convert, we divide our kg by the this number.
mConst :: Double
mConst = 1.48807874e-34

processPlanetData :: String -> String -> [(Planet, V3D)]
processPlanetData nStr dStr = zipWith mergeData nData dData
  where
    nData = map processLineN $ lines nStr
    processLineN = makeDataN . words
    makeDataN (name:dat) =
      let (m:px:py:pz:vx:vy:vz:_) = map read dat
          b                       = Body (mConst * m) (V3 px py pz)
          v0                      = V3 vx vy vz
      in  (Planet name 5 black b, v0)
    dData                        = map processLineD $ lines dStr
    processLineD                 = drop 1 . words
    mergeData (p,v0) (rad:col) =
      ( p { planetRadius = rConst * read rad, planetColor = colTup (read (unwords col)) }
      , v0)
    rConst = 0.04
    colTup (cr,cg,cb) = sRGB24 cr cg cb


main :: IO ()
main = do
  pData <- processPlanetData
    <$> readFile "data/planet_data.dat"
    <*> readFile "data/planet_display.dat"
  mapM_ print pData
  runManyMouse pData

-- runMany :: [(Planet, V3D)] -> IO ()
-- runMany planets = runTest (length planets) (w . pure ())
--   where
--     bwire = manyBody (map bTup planets) euler
--     w = arr (zipWith ($) planetMakers) . bwire
--     planetMakers = map (pMaker . fst) planets

runManyMouse :: [(Planet, V3D)] -> IO ()
runManyMouse planets = runTest (length planets + 1) wz
  where
    bwire = manyBody' (map bTup planets) verlet
    w = zipWith ($) planetMakers <$> bwire
    wz = proc e -> do
      bs <- w -< e
      zoomE <- filterE isJust -< zoomEvent <$> e
      zoom <- hold . accumE (*) 1 <|> pure 1 -< fromJust <$> zoomE
      returnA -< (zoom,bs)
    planetMakers = map (pMaker . fst) (planets ++ [extraPlanet])
    extraPlanet = (Planet "Bob" 0.1 white (Body 0 zero), zero)

    zoomEvent (RenderMouseDown _ RenderMouseWheelUp)    = Just (1/1.2)
    zoomEvent (RenderMouseDown _ RenderMouseWheelDown)  = Just (1.2)
    zoomEvent (RenderKeyDown (RenderKeyData c _))
      | c == ord '+'                                    = Just (1.2)
      | c == ord '-'                                    = Just (1/1.2)
      | otherwise                                       = Nothing
    zoomEvent _                                         = Nothing

manyBody' :: forall m e t s. (MonadFix m, Monoid e, HasTime t s, Fractional t)
    => [(Body, V3D)]          -- Initial body states and initial velocities
    -> Integrator             -- Integrator
    -> Wire s e m (Event RenderEvent) [Body]
manyBody' bodyList igr = proc e -> do
    rec
      bs <- bodyWires -< (bs, e)
    returnA -< bs
  where
    toWire :: (Body, V3D) -> Wire s e m ([Body], Event RenderEvent) Body
    toWire (b0, v0) = proc (bs,_) -> do
      bodyGs b0 v0 igr -< bs
    bodyWires = sequenceA $ map toWire bodyList ++ [mouseBody]
    mouseBody :: Wire s e m ([Body], Event RenderEvent) Body
    mouseBody = proc (_,e) -> do
      t <- realToFrac . (/5) <$> time -< ()
      let pos = 2.5 *^ V3 (sin t) (cos t) (sin ((t+1)/2) / 4)
      lastMouse <- hold . filterE isMouseEvent <|> pure emptyMouse-< e
      returnA -< case lastMouse of
        RenderMouseDown _ RenderMouseLeft -> Body 0.5 pos
        _ -> Body 0 pos
      -- returnA -< Body 0 1
    emptyMouse :: RenderEvent
    emptyMouse = RenderMouseUp (0,0) RenderMouseLeft


-- runFixed :: [(Planet, V3D)] -> IO ()
-- runFixed pData = runTest 9 (w . pure ())
--   where
--     bwire = manyFixedBody [sun] (map bTup pData) verlet
--     w = zipWith ($) planetMakers <$> bwire
--     ((Planet _ _ _ sun,_):planets) = pData
--     planetMakers = map (pMaker . fst) planets

-- runTwoBody :: (Planet,V3D) -> (Planet, V3D) -> IO ()
-- runTwoBody (p1@(Planet _ _ _ b1), v1) (p2@(Planet _ _ _ b2), v2) =
--     runTest 2 (w . pure ())
--   where
--     w = planetMaker <$> twoBody (b1,v1) (b2,v2) verlet
--     planetMaker (a,b) = [pMaker p1 a, pMaker p2 b]

-- runOneBody :: (Planet,V3D) -> IO ()
-- runOneBody (p@(Planet _ _ _ b0), v0) = runTest 1 (w . pure ())
--   where
--     w = map (pMaker p) <$> manyFixedBody [Body 1 zero] [(b0,v0)] verlet

runTest :: Int -> Wire (Timed Double ()) () Identity (Event RenderEvent) (Double, [Planet]) -> IO ()
runTest _ w =
#ifdef WINDOWS
  runBackend (glutBackend (1/30) 2.5 (600,600) (31,31,31)) (const . return $ ()) (uncurry PlanetList <$> w)
#else
  runBackend (sdlBackend (1/30) 30 (600,600) (31,31,31)) (const . return . return $ ()) (uncurry PlanetList <$> w)
#endif

runTestGNUPlot :: Int -> Wire (Timed Double ()) () IO (Event RenderEvent) [Planet] -> IO ()
runTestGNUPlot n w = do
  clearLogs 10
  runBackend (gnuPlotBackend 1 20000) (writeLog n) w

bTup :: (Planet, V3D) -> (Body, V3D)
bTup (Planet _ _ _ b0, v0) = (b0, v0)

pMaker :: Planet -> Body -> Planet
pMaker (Planet n r c _) = Planet n r c

clearLogs :: Int -> IO ()
clearLogs n = forM_ [0..(n-1)] $ \i ->
  writeFile
    ("out/planets_b" ++ show i ++ ".dat")
    ""

writeLog :: Int -> [Planet] -> IO ()
writeLog n planets = forM_ [0..(n-1)] $ \i ->
  appendFile
    ("out/planets_b" ++ show i ++ ".dat")
    ((++ "\n") . gnuplot $ planets !! i)

