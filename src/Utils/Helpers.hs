
module Utils.Helpers
  ( selects
  , wee
  , foldAcrossl
  , partition3
  , rotationDir
  , zipMapWithDefaults
  , unzipMap
  , isRight
  , zipTake
  ) where

import Data.List (foldl')
import qualified Data.Map as M
import Data.Foldable (foldr)
import Control.Arrow ((&&&))
import Prelude hiding (foldr)


{-# INLINE selects #-}
selects :: [a] -> [(a,[a])]
selects = go []
  where
   go _ [] = []
   go xs (y:ys) = (y,xs++ys) : go (y:xs) ys

wee :: IO ()
wee = mapM_ (putStrLn . flip replicate '.' . (6-) . abs) [-5..5]

{-# INLINE foldAcrossl #-}
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

-- {-# INLINE zipMapWithDefaults #-}
zipMapWithDefaults :: (Ord k) => (a -> b -> c) -> Maybe a -> Maybe b -> M.Map k a -> M.Map k b -> M.Map k c
zipMapWithDefaults f x0 y0 = M.mergeWithKey f' zx zy
  where
    f' _ x y = Just (x `f` y)
    zx = case y0 of
           Nothing -> const M.empty
           Just y' -> fmap (`f` y')
    zy = case x0 of
           Nothing -> const M.empty
           Just x' -> fmap (x' `f`)

unzipMap :: (Ord k) => M.Map k (a, b) -> (M.Map k a, M.Map k b)
unzipMap = fmap fst &&& fmap snd

{-# INLINE isRight #-}
isRight :: Either b a -> Bool
isRight (Right _) = True
isRight _ = False

zipTake :: [a] -> [b] -> ([(a,b)],([a],[b]))
zipTake [] ys = ([],([],ys))
zipTake xs [] = ([],(xs,[]))
zipTake (x:xs) (y:ys) = ((x,y):zs, (xs',ys'))
  where
    (zs,(xs',ys')) = zipTake xs ys
