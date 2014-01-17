
module Physics.Body
  ( Body(..)
  , bodyF
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

-- | Wire of a simple body under various forces
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

