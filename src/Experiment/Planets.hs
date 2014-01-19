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
--     (unlines . map (gnuplot . (!! i)) $ logs)

-- writeLogs :: [[Body]] -> Int -> IO ()
-- writeLogs logs n = forM_ [0..(n-1)] $ \i ->
--   writeFile
--     ("out/planets_b" ++ show i ++ ".dat")
--     (unlines . map (gnuplot . (!! i)) $ logs)



  -- writeFile "out/planets_b0.dat" . unlines . map (gnuplot . (!! 0)) $ logs
  -- writeFile "out/planets_b1.dat" . unlines . map (gnuplot . (!! 1)) $ logs
  -- writeFile "out/planets_b2.dat" . unlines . map (gnuplot . (!! 2)) $ logs
  -- where
  --   logs = execWriter logWriter
  --   logWriter = testWireRight
  --     50000
  --     (0.02 :: Double)
  --     (tell . return)
  --     (threeBody verlet :: (MonadFix m, HasTime t s) => Wire s String m () [Body])

    -- writeFile "out/planets_newton.dat" $ unlines (logs euler)
    -- writeFile "out/planets_verlet.dat" $ unlines (logs verlet)
  -- where
    -- logs i = execWriter $ logWriter i
    -- logWriter i = testWireRight
    --   10000
    --   (0.01 :: Double)
    --   (tell . return)
    --   (gnuplot <$> body x0 v0 xa i
    --       :: (HasTime t s, MonadFix m)
    --           => Wire s String m () String)
    -- x0 = zero
    -- v0 = V3 1   0 0.05
    -- xa = V3 0.2 1 0

-- main :: IO ()
-- main = SDL.withInit [SDL.InitEverything] $ do
--   screen <- SDL.setVideoMode 600 600 32 [SDL.SWSurface]
--   -- void $ go undefined screen clockSession planets
--   return ()

 -- where
 --   go = undefined

  -- go keysDown screen s w = do
  --   keysDown' <- parseEvents keysDown
  --   (x, w', s') <- stepSession_ w s keysDown'

  --   (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
  --       SDL.fillRect screen Nothing

  --   (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
  --       SDL.fillRect screen (Just $ SDL.Rect (round x) 0 50 50)

  --   SDL.flip screen
  --   go keysDown' screen s' w'



