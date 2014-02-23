{-# LANGUAGE CPP #-}

module Main where

import Control.Monad.Random
import Control.Wire.Unsafe.Event
import Control.Wire
import Linear.V3
import Data.Colour.Names
import Experiment.Battlefield.Team
import Data.Maybe (catMaybes)
import Experiment.Battlefield.Soldier
import Experiment.Battlefield.Types
import Prelude hiding               ((.),id)
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
      <$> (teamWire dim fl1 <$> getSplit)
      <*> (teamWire dim fl2 <$> getSplit)
  testStage $ simpleStage dim t1 t2
  where
    dim = (600,400)
    fl1 = TeamFlag red
    fl2 = TeamFlag blue
    -- counts = (9,5,3,3,4,2)
    -- genTeam' fl = genTeam dim fl counts

reportClasses :: IO ()
reportClasses = mapM_ (print . report) classScores
  where
    report :: Double -> (Double,Double,Int)
    report score = (1 / score, score, round (classWeight * score))
    classScores = map ((1 /) . classWorth) allClasses
    classWeight = 30 / sum classScores

simpleStage ::
     (Double, Double)
  -> TeamWire'
  -> TeamWire'
  -> Wire' () Stage
simpleStage dim@(w,h) t1w t2w = proc _ -> do
    rec
      b10 <- now -< b1s
      b20 <- now -< b2s
      let
        b1es = repeat NoEvent
        b2es = repeat NoEvent
      (team1@(Team _ t1ss t1as), t2ahits) <- t1w -< (team2, ((b10,b1es), t1ahits))
      (team2@(Team _ t2ss t2as), t1ahits) <- t2w -< (team1, ((b20,b2es), t2ahits))
    let
      sldrs = catMaybes (t1ss ++ t2ss)
      arts  = t1as ++ t2as
    returnA -< Stage dim sldrs arts
  where
    b1s = GotBase <$> [Base (V3 (w/5) (h/5) 0), Base (V3 (w/5) (4*h/5) 0)]
    b2s = GotBase <$> [Base (V3 (4*w/5) (h/5) 0), Base (V3 (4*w/5) (4*h/5) 0)]


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
