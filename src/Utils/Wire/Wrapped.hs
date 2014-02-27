module Utils.Wire.Wrapped where

-- import Data.Maybe             (catMaybes)
import Control.Monad             (zipWithM)
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Traversable          (sequence)
import Prelude hiding            ((.),id,sequence)
import Utils.Helpers             (zipMapWithDefaults)
import qualified Data.Map.Strict as M


wrappedWire :: (Monoid e, Monoid s, Monad m) => Wire s e m a b -> Wire s e m a (Wire s e m a b)
wrappedWire w' = mkGen $ \ds a -> do
  (_, w) <- stepWire w' ds (Right a)
  return (Right w, wrappedWire w)

dWireBox :: forall m e a b s. (Monoid s, Monad m) => a -> Wire s e m (Event [Wire s e m a b],[a]) [b]
dWireBox fill = wireBox fill . delay (NoEvent,[])

wireBox :: forall m e a b s. (Monoid s, Monad m) => a -> Wire s e m (Event [Wire s e m a b],[a]) [b]
wireBox fill = go []
  where
    go :: [Wire s e m a b] -> Wire s e m (Event [Wire s e m a b],[a]) [b]
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

dWireMap :: (Monoid s, Monad m, Ord k, Enum k)
    => a
    -> k
    -> Wire s e m (Event [Wire s e m a b], M.Map k a) (M.Map k b)
dWireMap fill k0 = wireMap fill k0 . delay (NoEvent, M.empty)

wireMap :: forall b e m a s k.
       (Monoid s, Monad m, Ord k, Enum k)
    => a
    -> k
    -> Wire s e m (Event [Wire s e m a b], M.Map k a) (M.Map k b)
wireMap fill k0 = go k0 M.empty
  where
    isRight (Right _) = True
    isRight _ = False
    go ::
         k
      -> M.Map k (Wire s e m a b)
      -> Wire s e m (Event [Wire s e m a b], M.Map k a) (M.Map k b)
    go k1 ws' = mkGen $ \ds (adds,as) -> do
        let
          zipped = zipMapWithDefaults f Nothing (Just fill) ws' as
          f w' a = stepWire w' ds (Right a)
        stepped <- sequence zipped
        let stepped' = M.filter (isRight . fst) stepped
            results  = fmap fst stepped'
            updateds = fmap snd stepped'
            (news,k2) =
              case adds of
                Event nws -> (newsmap,k')
                  where
                    nwsks = zip [k0..] nws
                    k' | null nws  = k1
                      | otherwise = succ . fst $ last nwsks
                    newsmap = M.fromList nwsks
                NoEvent   -> (M.empty, k1)
        return (sequence results, go k2 (updateds `M.union` news))

