
module Utils.Helpers
  ( selects
  , wee
  , foldAcrossl
  , partition3
  , rotationDir
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

partition3 :: (a -> Maybe Bool) -> [a] -> (([a],[a]),[a])
partition3 p = foldr select3 (([],[]),[])
  where
    select3 x ~((t,b),n) = case p x of
                             Just True  -> ((x:t,b),n)
                             Just False -> ((t,x:b),n)
                             _          -> ((t,b),x:n)

rotationDir :: Double -> Double -> Bool
rotationDir a1 a2 = dp < dn
  where
    (dp,dn) | a1 > a2   = (a2 + 2*pi - a1,a1 - a2)
            | otherwise = (a2 - a1,a1 + 2*pi - a2)


