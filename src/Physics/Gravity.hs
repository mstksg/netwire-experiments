{-# LANGUAGE Arrows #-}

module Physics.Gravity
  ( bodyG
  , bodyGs
  , twoBody
  , manyFixedBody
  ) where

import Control.Category
import Control.Monad.Writer.Strict
import Control.Wire
import Data.Traversable
import Linear.Metric
import Linear.Vector
import Physics.Body
import Physics.Integrator
import Physics.Physics
import Prelude hiding               ((.), id)

-- | Wire of a simple body under a single varying gravity source
--
bodyG :: (MonadFix m, Monoid e, HasTime t s)
    => Body       -- initial body state
    -> V3D        -- initial velocity
    -> Integrator -- integrator
    -> Wire s e m Body Body
bodyG b0 v0 igr = bGs . arr return
  where
    bGs = bodyGs b0 v0 igr

-- | Wire of a simple body under many gravitational sources
--
bodyGs :: (MonadFix m, Monoid e, HasTime t s)
    => Body       -- initial body state
    -> V3D        -- initial velocity
    -> Integrator -- integrator
    -> Wire s e m [Body] Body
bodyGs b0 v0 igr = proc others -> do
  rec
    let gravs = map (`bodyGravity` b) others
    b <- bF . delay [] -< gravs
  returnA -< b
  where
    bF = bodyF b0 v0 igr

-- | Two body system
--
twoBody :: (MonadFix m, Monoid e, HasTime t s)
    => (Body, V3D)
    -> (Body, V3D)
    -> Integrator
    -> Wire s e m () (Body,Body)
twoBody (b0,v0) (b0',v0') igr = proc _ -> do
  rec
    b1 <- bodyG b0  v0  igr -< b2
    b2 <- bodyG b0' v0' igr -< b1
  returnA -< (b1,b2)

-- | Many bodies under the influced of fixed sources
--
manyFixedBody :: (MonadFix m, Monoid e, HasTime t s)
    => [Body]
    -> [(Body, V3D)]
    -> Integrator
    -> Wire s e m () [Body]
manyFixedBody sources bodyList igr = sequenceA wireList
  where
    toWire (b0, v0) = bodyGs b0 v0 igr . pure sources
    wireList = map toWire bodyList

-- manyBody :: (MonadFix m, Monoid e, HasTime t s)
--     => [(Body, V3D)]          -- Initial body states and initial velocities
--     -> Integrator             -- Integrator
--     -> Wire s e m () [Body]
-- manyBody bodyList igr = seqArrow wireList
--   where
--     makeWire (b0, v0) (_, xs) = bodyGs b0 v0 igr . delay [] . seqArrow xs
--     wireList = zipWith makeWire bodyList (selects wireList)

-- seqArrow :: (Arrow r, ArrowLoop r) => [r a b] -> r a [b]
-- seqArrow []     = proc _ -> returnA -< []
-- seqArrow (r:rs) = proc a -> do
--   rec
--     b  <- r -< a
--     bs <- seqArrow rs -< a
--   returnA -< b : bs

-- manyBody bodyList igr = proc _ -> do
--   let
--     toWire (b0, v0) = bodyGs b0 v0 igr
--     bWires = map toWire bodyList
--     bSelects = selects bWires
--     mergeWires (bWire, otherWires) = bWire . Tr.sequenceA otherWires
--     wireList = map mergeWires bSelects
--     sequenced = Tr.sequenceA wireList
  -- rec
  --   wireList <- sequenced
  -- sequenced -<

-- bar :: (Floating a, Arrow r, Applicative (r ()), ArrowLoop r) => [a] -> r () [a]
-- bar xs = Tr.sequenceA foos
--   where
--     foos = zipWith (\y (_,ys) -> foo y . Tr.sequenceA ys) xs (selects foos)

-- manyBody bodyList igr = Tr.sequenceA wireList
--   where
--     toWire (b0, v0) = bodyGs b0 v0 igr
--     bWires = map toWire bodyList
--     bSelects = selects bWires
--     mergeWires (bWire, otherWires) = bWire . Tr.sequenceA otherWires
--     wireList = map mergeWires bSelects

-- threeBody :: (MonadFix m, Monoid e, HasTime t s)
--     => Integrator
--     -> Wire s e m () [Body]
-- threeBody = manyBody bodyList
--   where
--     body1 = (Body 1000 zero, zero)
--     body2 = (Body 0.9 (V3 50 0 0), V3 0.1 1 0.1)
--     body3 = (Body 1 (V3 0 60 0), V3 0.9 0.1 0.4)
--     bodyList = [body1,body2,body3]

-- | Force of gravity between bodies
--
bodyGravity ::
       Body -- attractor
    -> Body -- self
    -> V3D  -- gravitational force
bodyGravity (Body m1 r1) (Body m2 r2) = gravity 1 m1 r1 m2 r2

-- | Force of gravity
--
gravity ::
    (Fractional (v a), Floating a, Metric v, Additive v)
    => a    -- graviational constant
    -> a    -- mass of attractor
    -> v a  -- position of attractor
    -> a    -- mass of object
    -> v a  -- position of self
    -> v a  -- graviational force
gravity g m1 r1 m2 r2 = mag *^ signorm r
  where
    r = r1 ^-^ r2
    mag = g * m1 * m2 / (norm r ** 2)

