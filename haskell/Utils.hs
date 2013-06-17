module Utils (splitEvery) where

import qualified Data.List as L

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . L.unfoldr (Just . splitAt n)
