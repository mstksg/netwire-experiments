module Utils.Wire.Wrapped where

-- import Data.Maybe             (catMaybes)
import Control.Monad             (zipWithM)
import Control.Wire
import Control.Wire.Unsafe.Event
import Prelude hiding            ((.),id)


wrappedWire :: (Monoid e, Monoid s, Monad m) => Wire s e m a b -> Wire s e m a (Wire s e m a b)
wrappedWire w' = mkGen $ \ds a -> do
  (_, w) <- stepWire w' ds (Right a)
  return (Right w, wrappedWire w)

-- dWireBox' :: forall m e a b s. (Monoid s, Monad m) => a -> Wire s e m ((Event [Wire s e m a b], [Event ()]),[a]) [b]
-- dWireBox' fill = wireBox fill [] . delay ((NoEvent,[]),[])

-- dWireBox :: forall m e a b s. (Monoid s, Monad m) => a -> [Wire s e m a b] -> Wire s e m ((Event [Wire s e m a b], [Event ()]),[a]) [b]
-- dWireBox fill ws = wireBox fill ws . delay ((NoEvent,[]),[])

-- wireBox' :: forall m e a b s. (Monoid s, Monad m) => a -> Wire s e m ((Event [Wire s e m a b], [Event ()]),[a]) [b]
-- wireBox' fill = wireBox fill []

-- wireBox :: forall m e a b s. (Monoid s, Monad m) => a -> [Wire s e m a b] -> Wire s e m ((Event [Wire s e m a b], [Event ()]),[a]) [b]
-- wireBox fill = go
--   where
--     -- go :: [Wire s e m a b] -> Wire s e m ((Event (Wire s e m a b), [Event ()]),[a]) [b]
--     go ws = mkGen $ \ds ((adds,deletes),as) -> do
--       stepped <- zipWithM (\w' a' -> stepWire w' ds (Right a')) ws (as ++ repeat fill)
--       let
--         results :: [Either e b]
--         results = map fst stepped
--         updateds :: [Wire s e m a b]
--         updateds = catMaybes $ zipWith deletor (map snd stepped) (deletes ++ repeat NoEvent)
--         deletor :: Wire s e m a b -> Event () -> Maybe (Wire s e m a b)
--         deletor _ (Event _) = Nothing
--         deletor w NoEvent   = Just w
--         news :: [Wire s e m a b]
--         news = case adds of
--                  Event nws -> nws
--                  NoEvent -> []
--       return (sequence results, go (news ++ updateds))

dWireBox' :: forall m e a b s. (Monoid s, Monad m) => a -> Wire s e m (Event [Wire s e m a b],[a]) [b]
dWireBox' fill = wireBox fill [] . delay (NoEvent,[])

dWireBox :: forall m e a b s. (Monoid s, Monad m) => a -> [Wire s e m a b] -> Wire s e m (Event [Wire s e m a b],[a]) [b]
dWireBox fill ws = wireBox fill ws . delay (NoEvent,[])

wireBox' :: forall m e a b s. (Monoid s, Monad m) => a -> Wire s e m (Event [Wire s e m a b],[a]) [b]
wireBox' fill = wireBox fill []

wireBox :: forall m e a b s. (Monoid s, Monad m) => a -> [Wire s e m a b] -> Wire s e m (Event [Wire s e m a b],[a]) [b]
wireBox fill = go
  where
    -- go :: [Wire s e m a b] -> Wire s e m (Event (Wire s e m a b),[a]) [b]
    go ws' = mkGen $ \ds (adds,as) -> do
      stepped <- zipWithM (\w' a' -> stepWire w' ds (Right a')) ws' (as ++ repeat fill)
      let
        stepped' :: [(Either e b, Wire s e m a b)]
        stepped' = [ ws | ws@(Right _,_) <- stepped ]
        (results, updateds) = unzip stepped'
        news :: [Wire s e m a b]
        news = case adds of
                 Event nws -> nws
                 NoEvent -> []
      return (sequence results, go (news ++ updateds))
