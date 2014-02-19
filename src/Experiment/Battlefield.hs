{-# LANGUAGE CPP #-}

module Main where

import Control.Monad.Random
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Colour.Names
import Data.List                    (transpose)
import Experiment.Battlefield.Team
import Experiment.Battlefield.Types
import Linear
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
  stage <- evalRandIO $
    simpleStage dim <$> genTeam' fl1 <*> genTeam' fl2
  testStage stage
  where
    dim = (600,400)
    fl1 = TeamFlag red
    fl2 = TeamFlag blue
    counts = (5,5,5,5,5,5)
    genTeam' fl = genTeam dim fl counts

simpleStage ::
     (Double, Double)
  -> TeamWire'
  -> TeamWire'
  -> Wire' () Stage
simpleStage dim@(w,h) t1w t2w = proc _ -> do
    rec
      let
        (t1ahits,t2dhits) = hitWatcher t1as t2ds
        (t2ahits,t1dhits) = hitWatcher t2as t1ds
        t1douts           = borderWatcher t1ds
        t2douts           = borderWatcher t2ds
        t1devts           = zipWith (<>) t1douts t1dhits
        t2devts           = zipWith (<>) t2douts t2dhits
      team1@(Team _ t1as t1ds) <- t1w -< (team2, (t1ahits,t1devts))
      team2@(Team _ t2as t2ds) <- t2w -< (team1, (t2ahits,t2devts))
    returnA -< Stage dim (t1as ++ t2as) (t1ds ++ t2ds)
  where
    hitWatcher :: [Soldier] -> [Article] -> ([SoldierInEvents],[Event ()])
    hitWatcher as ds = (hitas, hitds)
      where
        hitMatrix = map hitad as
        hitad a   = map (collision a) ds
        collision (Soldier (PosAng ps _) _ _ _ _ _)
                  (Article (PosAng pa _) (ArticleAttack (Attack _ dmg)))
                  | norm (ps ^-^ pa) < 5  = Event [AttackedEvent dmg]
        collision _ _ = NoEvent
        hitas     = map mconcat hitMatrix
        hitds     = map ((() <$) . mconcat) (transpose hitMatrix)
    borderWatcher :: [Article] -> [Event ()]
    borderWatcher = map outOfBounds
      where
        outOfBounds (Article (PosAng (V3 x y _) _) (ArticleAttack _))
          | or [x < 0, y < 0, x > w, y > h] = Event ()
        outOfBounds _                       = NoEvent

testStage :: Wire' () Stage -> IO ()
testStage w =
#ifdef WINDOWS
  runBackend
    (glutBackend (1/30) 5 (600,600) (50,50,50))
    (const . return $ ())
    (w . pure ())
#else
  runBackend
    (sdlBackend 600 600 (50,50,50))
    (const . return . return $ ())
    (w . pure ())
#endif
