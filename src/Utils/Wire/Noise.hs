module Utils.Wire.Noise where

import Control.Wire
import Prelude hiding ((.),id)
import FRP.Netwire.Noise
import Linear.V3
import System.Random

-- definitely breaks FRP.  do not use!
--
noisePrim :: forall s e m g b. (RandomGen g, Random b) => g -> Wire s e m () b
noisePrim gen = mkSFN (const res)
  where
    res = (out, noisePrim gen')
    (out,gen') = random gen
    
-- definitely breaks FRP.  do not use!
--
noisePrimR :: forall s e m g b. (RandomGen g, Random b) => (b,b) -> g -> Wire s e m () b
noisePrimR range gen = mkSFN . const $ (out, noisePrimR range gen')
  where
    (out,gen') = randomR range gen


noiseDisc :: (Floating b, Monoid e, Monad m, RandomGen g, Random b, HasTime t s) => t -> b -> g -> Wire s e m a (V3 b)
noiseDisc t z gen = proc x -> do
    r <- hold . noiseR t (0,1) gr <|> pure ri-< x
    th <- hold . noiseR t (0,2*pi) gth <|> pure thi -< x
    returnA -< pickDisk r th
  where
    (ri,g') = randomR (0,1) gen
    (thi,g'') = randomR (0,2*pi) g'
    pickDisk r th = V3 (sqrtr * cos th) (sqrtr * sin th) z
      where
        sqrtr = sqrt r
    (gr,gth) = split g''
