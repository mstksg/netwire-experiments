{-# LANGUAGE CPP #-}

module Main where

import Control.Monad.Random
import Control.Wire
import Data.Colour.Names
import Experiment.Battlefield.Soldier
import Experiment.Battlefield.Stage
import Data.Default
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
  (t1,t2) <- evalRandIO $ (,)
      <$> (TeamData fl1 <$> getSplit)
      <*> (TeamData fl2 <$> getSplit)
  testStage $ stageWire def dim t1 t2
  where
    dim = (600,400)
    fl1 = TeamFlag red
    fl2 = TeamFlag blue
    -- counts = (9,5,3,3,4,2)

reportClasses :: IO ()
reportClasses = mapM_ (print . report) classScores
  where
    report :: Double -> (Double,Double,Int)
    report score = (1 / score, score, round (classWeight * score))
    classScores = map ((1 /) . classWorth) allClasses
    classWeight = 30 / sum classScores

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
    (const . return . return $ ())
    (w . pure ())
#endif
