{-# LANGUAGE Arrows #-}

import Prelude hiding ((.))
import Control.Category
import Control.Applicative
import Control.Arrow
import qualified Data.Traversable as Tr

-- instance Monad m => Functor (Kleisli m a) where
--   fmap f a = a >>> arr f

-- instance Monad m => Applicative (Kleisli m a) where
--   pure a = arr (const a)
--   (Kleisli a) <*> (Kleisli b) = \x ->

foo :: (Floating a, Arrow r, ArrowLoop r) => a -> r [a] a
foo x = proc xs -> arr (cos . (*x) . sum) -< xs

bar :: (Floating a, Arrow r, Applicative (r ()), ArrowLoop r) => [a] -> r () [a]
bar xs = Tr.sequenceA foos
  where
    foos = zipWith (\y (_,ys) -> foo y . Tr.sequenceA ys) xs (selects foos)

  -- where
  --   xSelects = selects xs
  --   toFoo (y, ys) = foo y . pure ys
  --   fooList = map toFoo xSelects

-- bar :: (Floating a, Arrow r, Applicative (r ()), ArrowLoop r) => [a] -> r () [a]
-- bar xs = Tr.sequenceA fooList
--   where
--     xSelects = selects xs
--     toFoo (y, ys) = foo y . pure ys
--     fooList = map toFoo xSelects

-- xs1 :: (Arrow r, ArrowLoop r, Floating a) => r () [a]
-- xs1 = proc _ -> (arr return) . foo 1 . xs1 -< ()

selects :: [a] -> [(a,[a])]
selects = go []
  where
   go _ [] = []
   go xs (y:ys) = (y,xs++ys) : go (y:xs) ys

main :: IO ()
main = print $ (bar :: [Double] -> (->) () [Double]) [1..10] ()
-- main = print $ (bar :: [Double] -> Kleisli Maybe () [Double]) undefined undefined
