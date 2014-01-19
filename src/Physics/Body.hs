{-# LANGUAGE Arrows #-}

module Physics.Body
  ( Body(..)
  , bodyF
  , bodyFPure
  , bodyFConstrained
  ) where

-- import FRP.Netwire
import Control.Category
import Control.Monad.Writer.Strict
import Control.Wire
import Data.Maybe                  (mapMaybe, isJust, fromMaybe)
import Linear.Metric
import Linear.V3
import Linear.Vector
import Physics.Integrator
import Physics.Physics
import Prelude hiding              ((.), id)
import Render.Backend.GNUPlot
import Utils.Wire.Interval

-- | Represents a point with mass and position
--
data Body = Body Double !V3D
  deriving (Show, Eq)

instance GNUPlottable Body where
  gnuplot (Body _ (V3 x y z)) = unwords . map show $ [x,y,z]

-- | Wire of a simple body and its velocity under a collection of forces
--
bodyFVel :: (MonadFix m, Monoid e, HasTime t s)
    => Body
    -> V3D
    -> Integrator
    -> Wire s e m [V3D] (Body, V3D)
bodyFVel (Body m x0) v0 igr = pairUp <$>
    runIntegrator igr x0 v0 . arr ((^/ m) . sum)
  where
    pairUp (x,v) = (Body m x, v)


-- | Wire of a simple body under a collection of forces
--
bodyF :: (MonadFix m, Monoid e, HasTime t s)
    => Body
    -> V3D
    -> Integrator
    -> Wire s e m [V3D] Body
bodyF b0 v0 igr = fst <$> bodyFVel b0 v0 igr

-- | Wire of a simple body under a constant set of forces
--
bodyFPure :: (MonadFix m, Monoid e, HasTime t s)
    => [V3D]
    -> Body
    -> V3D
    -> Integrator
    -> Wire s e m () Body
bodyFPure fs b0 v0 igr = bodyF b0 v0 igr . pure fs

-- | A body under rigid elastic "box" constraints.  The predicate function
-- is a collision function that tests if a collision has happened.  The
-- parameter in the case of a Just is the (unit) direction of the
-- **normal** to the collided surface.
--
bodyFConstrained :: (MonadFix m, Monoid e, HasTime Double s)
    => (V3D -> Maybe V3D)   -- Collision function
    -> Body
    -> V3D
    -> Integrator
    -> Wire s e m [V3D] Body
bodyFConstrained c b0@(Body m _) v0 igr = proc fs -> do
  rec
    let
      -- allfs = fs
      allfs    = impmag:fs
      momentum = m *^ vel
      impmag   = (imp `dot` (momentum * (-2))) *^ imp
    -- imps <- delay [] . impulses c -< x

    -- collisions <- hold . accumE (flip (:)) [] . became (isJust . c) -< x

    delayedX <- delay zero -< x

    imp <- holdTickAndAmplify . arr (posToImp <$>) . became (isJust . c) <|> pure zero -< delayedX
    -- ix' <- holdTickAndAmplify -< ix

    -- imp <-
    --   (^* 60) . posToImp <$> holdFor (1/60) . became (isJust . c) <|>
    --   pure zero
    --     -< delayedX

    -- (b@(Body _ x), vel) <- delay (b0, v0) . bodyFVel b0 v0 igr -< allfs
    (b@(Body _ x), vel) <- delay (b0, v0) . bodyFVel b0 v0 igr -< allfs
  returnA -< b
  where
    posToImp x = fromMaybe zero (c x)
  -- where
    -- newImpulse x = case c x of
    --                  Just _   -> undefined
    --                  Nothing  -> undefined

    -- addImp (imps,Nothing) = imps
    -- addImp (imps,Just cv0) = impulse' cv0 1 : imps


-- testCollide :: (MonadFix m, Monoid e, HasTime Double s)
--     => (V3D -> Maybe V3D)
--     -> Wire s e m Body (Body, Maybe V3D)
-- testCollide c = proc b@(Body _ x) -> returnA -< (b, c x)

-- impulses :: (MonadFix m, Monoid e, HasTime Double s)
--     => (V3D -> Maybe V3D)
--     -> Wire s e m V3D [V3D]
-- impulses c = proc x -> undefined -< undefined

impulses :: forall m e s. (MonadFix m, Monoid e, HasTime Double s)
    => (V3D -> Maybe V3D)
    -> Wire s e m V3D [V3D]
impulses c = mkSF (p [])
  where
    -- impulse magnitude, impulse direction, impulse duration
    p :: [(V3D, (V3D, Double))] -> s -> V3D -> ([V3D], Wire s e m V3D [V3D])
    p ps ds x =
      let dt  = dtime ds
          downed  = mapMaybe (countdown dt) ps
          downed' = case c x of
            Just iunit | not . any ((== iunit) . fst . snd) $ ps
              -> (iunit ^* 50, (iunit, dt)) : downed
            _ -> downed
      in (map fst downed', mkSF (p downed'))

    countdown dt (ivec, (iunit, idur)) =
      let down = idur - dt
      in  if down > 0
            then Just (ivec, (iunit, down))
            else Nothing

impulses' :: (MonadFix m, Monoid e, HasTime Double s)
    => Wire s e m (Event ()) ()
impulses' = undefined

-- impulse' :: (MonadFix m, Monoid e, HasTime Double s)
--     => V3D
--     -> Double
--     -> Wire s e m () V3D
-- impulse' dP dT = undefined

-- impulse' :: (MonadFix m, Monoid e, HasTime Double s)
--     => Wire s e m (Event (V3D, Double)) V3D
-- impulse' = undefined



-- impulse :: (MonadFix m, Monoid e, HasTime Double s)
--     => Wire s e m (Event (V3D, Double)) V3D
-- impulse = for dT . pure (dP ^/ dT) . asSoonAs <|> pure zero
--   where
--     dT = 1
--     dP = zero


    -- runIntegrator igr x0 v0 . arr ((^/ m))
  -- where
    -- thisBody = Body m
