
module Utils.Helpers
  ( selects
  , wee
  , foldAcrossl
  ) where

import Data.List (foldl')

selects :: [a] -> [(a,[a])]
selects = go []
  where
   go _ [] = []
   go xs (y:ys) = (y,xs++ys) : go (y:xs) ys

wee :: IO ()
wee = mapM_ (putStrLn . flip replicate '.' . (6-) . abs) [-5..5]

foldAcrossl :: (a -> b -> a) -> a -> [[b]] -> [a]
foldAcrossl f x = foldl' (zipWith f) (repeat x)

