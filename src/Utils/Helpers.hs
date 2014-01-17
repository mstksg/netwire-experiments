
module Utils.Helpers
  ( selects
  , wee
  ) where

selects :: [a] -> [(a,[a])]
selects = go []
  where
   go _ [] = []
   go xs (y:ys) = (y,xs++ys) : go (y:xs) ys

wee :: IO ()
wee = mapM_ (putStrLn . flip replicate '.' . (6-) . abs) [-5..5]
