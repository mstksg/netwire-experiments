module Render.Render where

import Control.Wire

newtype Backend s e m a = Backend {
    runBackend ::
         (a -> IO ())       -- render function
      -> Wire s e m () a    -- wire
      -> IO ()
  }
