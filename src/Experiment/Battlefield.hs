module Main where

import Control.Wire
import Control.Wire.Unsafe.Event
import Data.List                         (transpose)
import Experiment.Battlefield.Team
import Experiment.Battlefield.Types
import Linear
import Prelude hiding                    ((.),id)

main :: IO ()
main = return ()

simpleStage ::
     Double
  -> Double
  -> TeamWire'
  -> TeamWire'
  -> Wire' () Stage
simpleStage w h t1w t2w = proc _ -> do
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
    returnA -< Stage w h (t1as ++ t2as) (t1ds ++ t2ds)
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
