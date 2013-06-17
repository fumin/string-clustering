module ComputeDistances ((!),
                         insert,
                         computeDistances
                        ) where

-- The goal of this module is to provide `computeDistances` 
-- which given a list of objects, and a distance definition,
-- returns a Map of the distance between all possible pairs.
-- For a list of length N, the returned Map would have size N(N-1)/2.
-- 
-- Note that we use a size 2 tuple to represent a pair of objects.
-- In particular, we always store (a, b) in the Map if a > b.
-- Thus, to query against the result of our `computeDistances`,
-- we should always use our provided `ComputeDistances.!`
-- instead of the built-in `Map.!`.

import qualified Data.Map as Map
import qualified Control.Parallel.Strategies as P
import qualified Control.DeepSeq as DS

import qualified Utils as Utils

import qualified Debug.Trace as DT

(!) :: (Ord a, Num b) => Map.Map (a, a) b -> (a, a) -> b
(!) distances (a, b)
  | a >  b    = distances Map.! (a, b)
  | a == b    = 0
  | otherwise = distances ! (b, a)

insert :: (Ord a) => (a, a) -> b -> Map.Map (a, a) b -> Map.Map (a, a) b
insert (a, b) v distances
  | a > b     = Map.insert (a, b) v distances
  | a == b    = distances
  | otherwise = insert (b, a) v distances

--------------------------------------------
-- computeDistances
--------------------------------------------
comD :: (Ord a, DS.NFData a, DS.NFData b) => (a -> a -> b) -> [a] -> [((a, a), b)]
comD distanceDefinition a =
  concat $ P.parMap P.rdeepseq (\n -> subDist distanceDefinition n a) [1..(length a)]

-- `subDist dist 2 [a, b, c, d, e]` = [(dist b c), (dist b d), (dist b e)]
subDist :: (Ord a) => (a -> a -> b) -> Int -> [a] -> [((a, a), b)]
subDist distanceDefinition n a =
  map (\s -> if x > s then ((x, s), (distanceDefinition x s))
             else ((s, x), (distanceDefinition x s))) bs
  where (as,bs) = splitAt n a
        x = last as

list2Map :: (Ord a, DS.NFData a, DS.NFData b) =>
            [((a, a), b)] -> Map.Map (a, a) b
list2Map a =
  foldr (\as bs -> Map.union as bs)
        Map.empty
        (P.parMap P.rdeepseq l2m (Utils.splitEvery granularity a))
  where granularity = 100
        l2m l = foldr (\t -> insert (fst t) (snd t)) Map.empty l

computeDistances :: (Ord a, DS.NFData b, DS.NFData a) =>
                    (a -> a -> b) -> [a] -> Map.Map (a, a) b
computeDistances distanceDefinition a = list2Map $ comD distanceDefinition a
