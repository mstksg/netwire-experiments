module Utils.Wire.Noise where

import Control.Wire
import Prelude hiding ((.),id)
import System.Random

noisePrim :: (RandomGen g, Random b) => g -> Wire s e m () b
noisePrim gen = mkSFN (const res)
  where
    res = (out, noisePrim gen')
    (out,gen') = random gen
    
noisePrimR :: (RandomGen g, Random b) => (b,b) -> g -> Wire s e m () b
noisePrimR range gen = mkSFN . const $ (out, noisePrimR range gen')
  where
    (out,gen') = randomR range gen

