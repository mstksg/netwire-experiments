module Main where

import Linear.V3
import Control.Wire
import Experiment.Battlefield.Types
import System.Random

main :: IO ()
main = return ()

genSoldier ::
       V3 Double        -- initial position
    -> Maybe TeamFlag   -- flag
    -> SoldierBody      -- body
    -> SoldierWeapon    -- weapon
    -> SoldierMount     -- mount
    -> StdGen           -- entropy source
    -> Wire s e m ([Soldier], SoldierInEvents) (Soldier, SoldierOutEvents)
genSoldier x0 fl bod weap mnt gen = undefined
