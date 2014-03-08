{-# LANGUAGE CPP #-}

module Main where

import Control.Monad.Random
import Control.Wire
import Data.Colour.Names
import Experiment.Battlefield.Soldier
import Experiment.Battlefield.Stage
import Data.Map.Strict ((!))
import Experiment.Battlefield.Types
import Prelude hiding                 ((.),id)
import Render.Render

#ifdef WINDOWS
import Render.Backend.GLUT
import Experiment.Battlefield.Instances.GLUT    ()
#else
import Render.Backend.SDL
import Experiment.Battlefield.Instances.SDL ()
#endif

main :: IO ()
main = do
  reportClasses
  g <- getStdGen
  testStage $ stageWireLoop dim t1fl t2fl g
  where
    dim = (600,400)
    t1fl = TeamFlag red
    t2fl = TeamFlag blue
    -- counts = (9,5,3,3,4,2)

reportClasses :: IO ()
reportClasses = mapM_ (print . report) classScores
  where
    report :: Double -> (Double,Double,Int)
    report score = (1 / score, score, round (classWeight * score))
    classScores = map ((1 /) . (normedClassWorths !)) allClasses
    classWeight = 100 / sum classScores

testStage :: Wire' () Stage -> IO ()
testStage w =
#ifdef WINDOWS
  runBackend
    (glutBackend (1/45) 1 (1500,800) (50,50,50))
    (const . return $ ())
    (w . pure ())
#else
  runBackend
    (sdlBackend (1/45) 30 (600,600) (50,50,50))
    (const . const . return . return $ ())
    (w . pure ())
#endif
