{-# LANGUAGE TupleSections #-}

module Utils.Wire.Misc where

import Control.Wire

-- might break FRP.
--
couple :: forall m e s a b. (Monoid s, Monad m) => Wire s e m () b -> Wire s e m (Event a) (Event (a,b))
couple = go
  where
    go :: Wire s e m () b -> Wire s e m (Event a) (Event (a,b))
    go w' = mkGen $ \ds e -> do
      (mx, w) <- stepWire w' ds (Right ())
      case mx of
        Right mx' -> do
          let e' = (,mx') <$> e
          return (Right e', go w)
        Left err ->
          return (Left err, go w)

-- breaks FRP, do not use!
-- For example, the if signal is Just on [t0,t) and Nothing on [t,inf), it
-- does not make sense to have a "last" Just value because there is no
-- maximal time.  The good solution would be to have some kind of discrete
-- sampler send events on Just, and use hold.
-- Oh well.
--
holdJust :: a -> Wire s e m (Maybe a) a
holdJust i = mkPureN $ \x ->
  case x of
    Just x' -> (Right x', holdJust x')
    Nothing -> (Right i , holdJust i)
