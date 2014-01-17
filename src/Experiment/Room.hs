
-- import Control.Wire
-- import Utils.Helpers     (wee)
import Control.Category
import Control.Monad
import Control.Monad.Fix
import Data.Traversable
import FRP.Netwire
import Linear.V3
import Linear.Vector
import Physics
import Prelude hiding       ((.), id)
import Utils.Output.GNUPlot
import Utils.Wire.TestWire

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
runTest n =
  testWire'
    (60*2)
    ((1/60) :: Double)
    -- (writeLog n)
    (either print (writeLog n))

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
    wall (V3 _ _ z) = z < 0