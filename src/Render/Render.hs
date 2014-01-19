module Render.Render where

import Control.Wire

newtype Backend s e m r a = Backend {
    runBackend ::
         (a -> r)           -- render function
      -> Wire s e m () a    -- wire
      -> IO ()
  }
