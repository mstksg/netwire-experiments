module Utils.Wire.LogWire (logWire) where

import Prelude hiding              ((.), id)
import Control.Monad.Writer.Strict
import Control.Wire
-- import FRP.Netwire

logWire :: (Monoid e, Real t) => Int -> t -> Wire (Timed t ()) e (Writer [b]) () b -> [b]
logWire n dt w0 = execWriter $ go n (countSession dt <*> pure ()) w0
  where
    go :: (Monoid e, Real t) => Int -> Session (Writer [b]) (Timed t ()) -> Wire (Timed t ()) e (Writer [b]) () b -> Writer [b] ()
    go i s' w'  | i <= 0 = return ()
                | otherwise = do
      (ds, s) <- stepSession s'
      (mx, w) <- stepWire w' ds (Right ())
      tell $ either (const []) return mx
      go (i-1) s w
