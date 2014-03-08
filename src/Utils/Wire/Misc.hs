{-# LANGUAGE TupleSections #-}

module Utils.Wire.Misc where

-- import Utils.Helpers             (zipMapWithDefaults)
-- import qualified Data.Map.Strict as M
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Maybe                   (isJust, fromJust)
import Prelude hiding               ((.),id)

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

curated :: Wire s e m (Event [a], a -> Maybe b) [b]
curated = go []
  where
    go xs = mkSFN $ \(news, f) ->
              case news of
                NoEvent  ->
                  let (out,next) = sortOut f xs
                  in  (out, go next)
                Event ys ->
                  let (out,next) = sortOut f (xs ++ ys)
                  in  (out, go next)

    sortOut f xs = (map fromJust out,next)
      where
        mapped = map (f &&& id) xs
        xs'    = filter (isJust . fst) mapped
        (out,next) = unzip xs'

zipWires :: (Monad m, Monoid e)
    => [Wire s e m a b]
    -> Wire s e m [a] [b]
zipWires = go
  where
    go [] = pure []
    go (w:ws) = proc inputs -> do
      case inputs of
        (x:xs) -> do
          w' <- w -< x
          ws' <- go ws -< xs
          returnA -< w':ws'
        [] -> do
          returnA -< error "zipped wire with no input"

zipWiresWithDefault :: (Monad m, Monoid e)
    => [Wire s e m a b]
    -> a
    -> Wire s e m [a] [b]
zipWiresWithDefault w0s x0 = go w0s
  where
    go [] = pure []
    go (w:ws) = proc inputs -> do
      case inputs of
        (x:xs) -> do
          w' <- w -< x
          ws' <- go ws -< xs
          returnA -< w':ws'
        [] -> do
          w' <- w -< x0
          ws' <- go ws -< repeat x0
          returnA -< w':ws'

zipEvents :: (Monad m, Monoid e, Semigroup b)
    => [Wire s e m a (Event b)]
    -> Wire s e m [a] (Event b)
zipEvents ws = mconcat <$> zipWires ws

delayM :: Monoid a => Wire s e m a a
delayM = delay mempty


-- mergeEventMap :: (Ord k, Semigroup a) => M.Map k (Event a) -> M.Map k (Event a) -> M.Map k (Event a)
-- mergeEventMap = zipMapWithDefaults (<>) (Just NoEvent) (Just NoEvent)
