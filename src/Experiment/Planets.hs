-- import Control.Monad             (void)
-- import Data.Traversable
-- import FRP.Netwire
-- import Linear.Metric
-- import Utils.Wire.LogWire
-- import qualified Graphics.UI.SDL as SDL
import Control.Category
import Control.Monad.Writer.Strict
import Control.Wire
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

processPlanetData :: String -> [(Body, V3D)]
processPlanetData = map processLine . lines
  where
    processLine = makeData . map read . drop 1 . words
    makeData (m:px:py:pz:vx:vy:vz:_) =
      ( Body (mConst * m) (V3 px py pz)
      , V3 vx vy vz )

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
  where
    b0 = (Body 1 (V3 4 0 0)   , V3 0 0.5 0 )
    b1 = (Body 3 (V3 (-1) 0 0), V3 0 1 0   )
    b2 = (Body 3 (V3 1 0 0)   , V3 0 (-1) 0)

runMany :: [(Body, V3D)] -> IO ()
runMany planets = runTest (length planets) $ manyBody planets verlet

runFixed :: [(Body, V3D)] -> IO ()
runFixed pData = runTest 9 $ manyFixedBody [sun] planets verlet
  where
    ((sun,_):planets) = pData

runTwoBody :: (Body,V3D) -> (Body, V3D) -> IO ()
runTwoBody b1 b2 = runTest 2 $ arr (\(a,b) -> [a,b]) . twoBody b1 b2 verlet

runOneBody :: (Body,V3D) -> IO ()
runOneBody b = runTest 1 w
  where
    w = manyFixedBody [Body 1 zero] [b] verlet

runFourBody :: (Body,V3D) -> (Body,V3D) -> (Body,V3D) -> (Body,V3D) -> IO ()
runFourBody b1 b2 b3 b4 =
  runTest 4 $ arr (\(a,b,c,d) -> [a,b,c,d]) . fourBody b1 b2 b3 b4 verlet

runTest :: Int -> Wire (Timed Double ()) String IO () [Body] -> IO ()
runTest n w = do
    clearLogs 10
    runBackend (gnuPlotBackend 1 20000) (writeLog n) w

clearLogs :: Int -> IO ()
clearLogs n = forM_ [0..(n-1)] $ \i ->
  writeFile
    ("out/planets_b" ++ show i ++ ".dat")
    ""

writeLog :: Int -> [Body] -> IO ()
writeLog n bodies = forM_ [0..(n-1)] $ \i ->
  appendFile
    ("out/planets_b" ++ show i ++ ".dat")
    ((++ "\n") . gnuplot $ bodies !! i)

