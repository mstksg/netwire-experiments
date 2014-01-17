{-# LANGUAGE Arrows #-}

module Physics.Body
  ( Body(..)
  , bodyF
  , bodyFPure
  , bodyFConstrained
  ) where

import Control.Category
import Control.Monad.Writer.Strict
import Control.Wire
import Linear.Vector
import Linear.V3
import Physics.Integrator
import Utils.Output.GNUPlot
import Physics.Physics
import Prelude hiding               ((.), id)

-- | Represents a point with mass and position
--
data Body = Body Double !V3D
  deriving (Show, Eq)

instance GNUPlottable Body where
  gnuplot (Body _ (V3 x y z)) = unwords . map show $ [x,y,z]

-- | Wire of a simple body under a collection of forces
--
bodyF :: (MonadFix m, Monoid e, HasTime t s)
    => Body
    -> V3D
    -> Integrator
    -> Wire s e m [V3D] Body
bodyF (Body m x0) v0 igr = thisBody <$>
    runIntegrator igr x0 v0 . arr ((^/ m) . sum)
  where
    thisBody = Body m

-- | Wire of a simple body under a constant set of forces
--
bodyFPure :: (MonadFix m, Monoid e, HasTime t s)
    => [V3D]
    -> Body
    -> V3D
    -> Integrator
    -> Wire s e m () Body
bodyFPure fs b0 v0 igr = bodyF b0 v0 igr . pure fs

bodyFConstrained :: (MonadFix m, Monoid e, HasTime Double s)
    => (V3D -> Bool)
    -> Body
    -> V3D
    -> Integrator
    -> Wire s e m [V3D] Body
bodyFConstrained c (Body m x0) v0 igr = proc fs -> do
  rec
    b@(Body _ x) <- delay (Body m x0) . bodyF (Body m x0) v0 igr -< imp:fs
    imp <- impulse -< undefined
  returnA -< b

impulse :: (MonadFix m, Monoid e, HasTime Double s)
    => Wire s e m (Event (V3D, Double)) V3D
impulse dP dT = for dT . pure (dP ^/ dT) . now <|> pure zero


    -- runIntegrator igr x0 v0 . arr ((^/ m))
  -- where
    -- thisBody = Body m
