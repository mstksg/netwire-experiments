{-# LANGUAGE TupleSections #-}

module Utils.Wire.Misc where

import Control.Wire

couple :: forall m e s a b. (Monoid s, Monad m) => Wire s e m () b -> Wire s e m (Event a) (Event (a,b))
couple = go
  where
    -- go :: Wire s e m () b -> Wire s e m (Event a) (Event (a,b))
    go w' = mkGen $ \ds e -> do
      (mx, w) <- stepWire w' ds (Right ())
      case mx of
        Right mx' -> do
          let e' = (,mx') <$> e
          return (Right e', go w)
        Left err ->
          return (Left err, go w)

