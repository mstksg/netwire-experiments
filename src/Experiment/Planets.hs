module Main where

-- import Control.Monad             (void)
-- import FRP.Netwire
-- import Linear.Metric
-- import Utils.Wire.LogWire
-- import qualified Graphics.UI.SDL as SDL
import Control.Category
import Control.Monad.Writer.Strict
import Control.Wire                 as W
import Data.Traversable
import Data.Word
import Linear.V2
import Linear.V3
import Linear.Vector
import Physics
import Prelude hiding               ((.), id)
import Render.Backend.GNUPlot
import Render.Backend.SDL
import Render.Render
import Render.Sprite
import Render.Surface
import qualified Graphics.UI.SDL    as SDL

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
      in  (Planet name 5 (0,0,0) b, v0)
    dData                        = map processLineD $ lines dStr
    processLineD                 = drop 1 . words
    mergeData (p,v0) (rad:col) =
      ( p { planetRadius = rConst * read rad, planetColor = read (unwords col) }
      , v0)
    rConst = 0.04


data Planet = Planet  { planetName    :: String
                      , planetRadius  :: Double
                      , planetColor   :: (Word8,Word8,Word8)
                      , planetBody    :: Body
                      } deriving (Show)

newtype PlanetList = PlanetList [Planet]

instance GNUPlottable Planet where
  gnuplot (Planet _ _ _ b) = gnuplot b

instance HasSprite Planet where
  toSprite (Planet _ r c (Body _ (V3 x y _))) =
    Sprite (V2 x y) (Circle r Filled) c

instance HasSurface PlanetList where
  toSurface (PlanetList ps) =
    Surface zero idTrans (map (EntSprite . toSprite) ps)

instance SDLRenderable PlanetList where
  renderSDL scr = mapM_ (renderSDL scr) . sList
    where
      sList pl = toSpriteList ctr (transScale scale) (toSurface pl)
      ht       = fromIntegral $ SDL.surfaceGetHeight scr
      wd       = fromIntegral $ SDL.surfaceGetHeight scr
      ctr      = V2 ht wd ^/ 2
      scale    = ht / 20


main :: IO ()
main = do
  pData <- processPlanetData
    <$> readFile "data/planet_data.dat"
    <*> readFile "data/planet_display.dat"
  mapM_ print pData
  runManyMouse pData

runMany :: [(Planet, V3D)] -> IO ()
runMany planets = runTest (length planets) (w . pure ())
  where
    bwire = manyBody (map bTup planets) euler
    w = arr (zipWith ($) planetMakers) . bwire
    planetMakers = map (pMaker . fst) planets

runManyMouse :: [(Planet, V3D)] -> IO ()
runManyMouse planets = runTest (length planets + 1) w
  where
    bwire = manyBody' (map bTup planets) verlet
    w = zipWith ($) planetMakers <$> bwire
    planetMakers = map (pMaker . fst) (planets ++ [extraPlanet])
    extraPlanet = (Planet "Bob" 0.1 (255,255,255) (Body 0 zero), zero)

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


runFixed :: [(Planet, V3D)] -> IO ()
runFixed pData = runTest 9 (w . pure ())
  where
    bwire = manyFixedBody [sun] (map bTup pData) verlet
    w = zipWith ($) planetMakers <$> bwire
    ((Planet _ _ _ sun,_):planets) = pData
    planetMakers = map (pMaker . fst) planets

runTwoBody :: (Planet,V3D) -> (Planet, V3D) -> IO ()
runTwoBody (p1@(Planet _ _ _ b1), v1) (p2@(Planet _ _ _ b2), v2) =
    runTest 2 (w . pure ())
  where
    w = planetMaker <$> twoBody (b1,v1) (b2,v2) verlet
    planetMaker (a,b) = [pMaker p1 a, pMaker p2 b]

runOneBody :: (Planet,V3D) -> IO ()
runOneBody (p@(Planet _ _ _ b0), v0) = runTest 1 (w . pure ())
  where
    w = map (pMaker p) <$> manyFixedBody [Body 1 zero] [(b0,v0)] verlet

runTest :: Int -> Wire (Timed Double ()) () IO (Event RenderEvent) [Planet] -> IO ()
runTest _ = runTestSDL
-- runTest n = runTestGNUPlot n

runTestGNUPlot :: Int -> Wire (Timed Double ()) () IO (Event RenderEvent) [Planet] -> IO ()
runTestGNUPlot n w = do
  clearLogs 10
  runBackend (gnuPlotBackend 1 20000) (writeLog n) w

runTestSDL :: Wire (Timed Double ()) () IO (Event RenderEvent) [Planet] -> IO ()
runTestSDL w =
  runBackend (sdlBackend 600 600 (31,31,31)) (const . return . return $ ()) (PlanetList <$> w)

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

