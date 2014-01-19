-- import Control.Monad             (void)
-- import Data.Traversable
-- import FRP.Netwire
-- import Linear.Metric
-- import Utils.Wire.LogWire
-- import qualified Graphics.UI.SDL as SDL
import Control.Category
import Control.Monad.Writer.Strict
import Control.Wire
import Data.Word
import Linear.V3
import Linear.Vector
import Physics
import Prelude hiding               ((.), id)
import Render.Backend.GNUPlot
import Render.Backend.SDL
import Render.Render

-- | What is this number?  Well, we want our graviational constant to be 1,
-- so we normalize with our time unit being a day and our distance unit
-- being an AU.  Our mass unit then must be 6.720074483812448e33 kg.  To
-- convert, we divide our kg by the this number.
mConst :: Double
mConst = 1.48807874e-34

processPlanetData :: String -> [(Planet, V3D)]
processPlanetData = map processLine . lines
  where
    processLine = makeData . words
    makeData (name:dat) =
      let (m:px:py:pz:vx:vy:vz:_) = map read dat
          b                       = Body (mConst * m) (V3 px py pz)
          v0                      = V3 vx vy vz
      in  (Planet name 1 (0,0,0) b, v0)

data Planet = Planet  { planetName    :: String
                      , planetRadius  :: Double
                      , planetColor   :: (Word8,Word8,Word8)
                      , planetBody    :: Body
                      } deriving (Show)

instance GNUPlottable Planet where
  gnuplot (Planet _ _ _ b) = gnuplot b

main :: IO ()
main = do
  pData <- processPlanetData <$> readFile "data/planet_data.dat"
  let
    sun:mercury:venus:earth:mars:jupiter:saturn:_ = pData
  mapM_ print pData
  runMany pData
  -- runMany [sun,venus,jupiter,saturn]
  -- runFixed pData
  -- runTwoBody sun jupiter
  -- runOneBody b0
  -- runFourBody sun venus jupiter saturn
  -- where
  --   b0 = (Body 1 (V3 4 0 0)   , V3 0 0.5 0 )
  --   b1 = (Body 3 (V3 (-1) 0 0), V3 0 1 0   )
  --   b2 = (Body 3 (V3 1 0 0)   , V3 0 (-1) 0)

runMany :: [(Planet, V3D)] -> IO ()
runMany planets = runTest (length planets) w
  where
    bwire = manyBody (map bTup planets) verlet
    w = arr (zipWith ($) planetMakers) . bwire
    planetMakers = map (pMaker . fst) planets

runFixed :: [(Planet, V3D)] -> IO ()
runFixed pData = runTest 9 w
  where
    bwire = manyFixedBody [sun] (map bTup pData) verlet
    w = zipWith ($) planetMakers <$> bwire
    ((Planet _ _ _ sun,_):planets) = pData
    planetMakers = map (pMaker . fst) planets

runTwoBody :: (Planet,V3D) -> (Planet, V3D) -> IO ()
runTwoBody (p1@(Planet _ _ _ b1), v1) (p2@(Planet _ _ _ b2), v2) =
    runTest 2 w
  where
    w = planetMaker <$> twoBody (b1,v1) (b2,v2) verlet
    planetMaker (a,b) = [pMaker p1 a, pMaker p2 b]

runOneBody :: (Planet,V3D) -> IO ()
runOneBody (p@(Planet _ _ _ b0), v0) = runTest 1 w
  where
    w = map (pMaker p) <$> manyFixedBody [Body 1 zero] [(b0,v0)] verlet

runTest :: Int -> Wire (Timed Double ()) String IO () [Planet] -> IO ()
runTest n w = do
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

