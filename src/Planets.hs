{-# LANGUAGE Arrows, RankNTypes, ScopedTypeVariables #-}

import Prelude hiding ((.), id)
-- import Control.Monad (void)
import Control.Monad.Writer.Strict
import Control.Category
import Control.Wire
import FRP.Netwire
-- import qualified Graphics.UI.SDL as SDL
-- import Data.Traversable
import Linear.V3
import Utils.Output.GNUPlot
-- import Utils.Wire.LogWire
import Utils.Wire.TestWire
import Linear.Metric
import Linear.Vector

type V3D = V3 Double

data Body = Body V3D
  deriving (Show)

instance GNUPlottable Body where
  gnuplot (Body (V3 x y z)) = unwords . map show $ [x,y,z]

main :: IO ()
main = writeFile "out/planets.dat" $ unlines logs
  where
    logs = execWriter logWriter
    logWriter = testWireRight
      10000
      (0.005 :: Double)
      (tell . return)
      (gnuplot <$> body
          :: (Monad m, HasTime t s, MonadFix m)
              => Wire s String m () String)


-- logWire 10000
--       (0.005 :: Double)
--       (gnuplot <$> body :: (HasTime t s, MonadFix m, Show t) => Wire s String m () String)

-- plotOutput :: (Monad m, HasTime t s, Show t) => Wire s e m a Body -> Wire s e m a String
-- plotOutput = gnuplot
--   where
--     format t (Body (V2 x y)) = unwords [show x, show y]

-- plotOutput :: (Monad m, HasTime t s, Show t) => Wire s e m a Body -> Wire s e m a String
-- plotOutput b = format <$> time <*> b
--   where
--     format t (Body (V2 x y)) = unwords [show x, show y]

-- addTime :: (Monad m, HasTime t s) => Wire s e m a b -> Wire s e m a (t,b)
-- addTime w = (,) <$> time <*> w

-- main :: IO ()
-- main = SDL.withInit [SDL.InitEverything] $ do
--   screen <- SDL.setVideoMode 600 600 32 [SDL.SWSurface]
--   -- void $ go undefined screen clockSession planets
--   return ()

 -- where
 --   go = undefined

  -- go keysDown screen s w = do
  --   keysDown' <- parseEvents keysDown
  --   (x, w', s') <- stepSession_ w s keysDown'

  --   (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
  --       SDL.fillRect screen Nothing

  --   (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
  --       SDL.fillRect screen (Just $ SDL.Rect (round x) 0 50 50)

  --   SDL.flip screen
  --   go keysDown' screen s' w'

-- planets :: (Monad m, Monoid e) => Wire s e m () [Body]
-- planets = proc _ -> do
--   rec
--     bodies <- body
--   returnA undefined


-- stepWires :: Monad m => Wire e m [Wire e m () b] [(b, Wire e m () b)]
-- stepWires = mkFixM $ \dt objects -> do
--   stepped <- mapM (\o -> stepWire o dt ()) objects
--   return $ Right [ (o, w') | (Right o, w') <- stepped ]

body :: (MonadFix m, Monoid e, HasTime t s) => Wire s e m () Body
body = Body <$> proc _ -> do
  rec
    acc <- acceleration -< pos
    vel <- velocity (V3 1 0 0.01) -< acc
    pos <- position zero -< vel
  returnA -< pos



-- position zero . velocity zero . acceleration . pure (pure 1)

-- largeUfo = proc _ -> do
--   pos <- require onScreen . ufoPos -< ()
--   shotsFired <- shoot <|> pure [] -< pos

  -- returnA -< (UFO pos Small, shotsFired)
position :: (Monad m, Monoid e, HasTime t s) => V3D -> Wire s e m V3D V3D
position = integral

velocity :: (Monad m, Monoid e, HasTime t s) => V3D -> Wire s e m V3D V3D
velocity = integral

acceleration :: (Monad m, Monoid e) => Wire s e m V3D V3D
acceleration = arr gravity
  where
    gravity v = mag *^ signorm r
      where
        mag = 1 / (norm r ** 2)
        r = V3 0.2 1 0 ^-^ v

