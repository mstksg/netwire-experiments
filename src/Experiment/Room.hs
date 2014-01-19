
-- import Control.Wire
-- import Render.Render
-- import Utils.Helpers       (wee)
import Control.Category
import Control.Monad
import Control.Monad.Fix
import Data.Traversable
import FRP.Netwire
import Linear.V3
import Linear.Vector
import Physics
import Prelude hiding         ((.), id)
import Render.Backend.GNUPlot
import Render.Render

fr :: Double
fr = 60

main :: IO ()
main = do
  clearLogs bCount
  runTest
    bCount
    (roomBodies roomGravity roomBodyList verlet)
  where
    roomGravity = V3 0 0 (-2)
    roomBodyList = [(Body 1 zero, V3 1 1 1)]
    bCount = length roomBodyList

runTest :: Int -> Wire (Timed Double ()) String IO () [Body] -> IO ()
runTest n w = do
    clearLogs 10
    runBackend (gnuPlotBackend (1/fr) (round (fr*6))) (writeLog n) w

clearLogs :: Int -> IO ()
clearLogs n = forM_ [0..(n-1)] $ \i ->
  writeFile
    ("out/room_b" ++ show i ++ ".dat")
    ""

writeLog :: Int -> [Body] -> IO ()
writeLog n bodies = forM_ [0..(n-1)] $ \i ->
  appendFile
    ("out/room_b" ++ show i ++ ".dat")
    ((++ "\n") . gnuplot $ bodies !! i)

roomBodies :: (MonadFix m, Monoid e, HasTime Double s)
    => V3D
    -> [(Body, V3D)]
    -> Integrator
    -> Wire s e m () [Body]
roomBodies grav bodies igr = sequenceA $ map makeWire bodies
  where
    makeWire (b0, v0) = bodyFConstrained wall b0 v0 igr . pure [grav]
    wall (V3 x _ z) | z < 0     = Just (V3 0 0 1)
                    | z > 1     = Just (V3 0 0 (-1))
                    | x < 0     = Just (V3 1 0 0)
                    | x > 1.5   = Just (V3 (-1) 0 0)
                    | otherwise = Nothing
