module ComputeDistances ((!),
                         insert,
                         computeDistances,
                         computeDistances1) where

-- Experiment results on computing the mutual distances of 2000 strings:
--
-- | Implementation                          | Time spent (seconds)
-- | computeDistances (with lists)           | 13
-- | computeDistances1 (good implementation) | 16
-- | computeDistances1 (bad implementation)  | 26
--
-- Lessons learned:
-- * Using `force` or `rdeepseq` where required is most important. Not using it at
--   the exact place needed may double the time spent or even turn the programme
--   to a single threaded one!
-- * Fizzled sparks can be avoided by setting a coarser granularity.
--   Take the following code for example:
--
--   runEval $ do
--     a <- rpar $ force $ someComputation first_half       -- thread 1
--     b <- rpar $ force $ anotherComputation second_half   -- thread 2
--     rseq a
--     rseq b
--     return $ a + b                                       -- main thread
--   where (first_half,second_half) = splitAt ((length some_list) `div` 2) some_list
--
--   If the cost of the main thread directly computing
--   `a` and then `b` and then `a + b` is lesser than the cost of creating the
--   sparks of thread 1 and 2, the sparks of thread 1 and 2 would be fizzled.
--   Apparently, one way to avoid this is to not over parallelize
--   when `some_list` is too short.
-- * The relationship between # of GC'ed sparks and performance is a bit vague.
--   While the % of GC'ed sparks for `computeDistances` with granularity 10 is as
--   high as 85%, whereas `computeDistances1` with a bad implementation of
--   `distanceBetween` has only 30%, the performance of `computeDistances` is still
--   about twice as better then `computeDistances1`

import qualified Data.Map as Map
import qualified Control.Parallel.Strategies as P
import qualified Control.DeepSeq as DS

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

list2Map :: (Ord a) => [((a, a), b)] -> Map.Map (a, a) b
list2Map a
  | granularity > (length a) = foldr (\t -> insert (fst t) (snd t)) Map.empty a
  | otherwise = P.runEval $ do
                  as' <- P.rpar $ list2Map as
                  bs' <- P.rpar $ list2Map bs
                  P.rseq as'
                  P.rseq bs'
                  return $ Map.union as' bs'
  where (as,bs) = splitAt ((length a) `div` 2) a
        granularity = 10
computeDistances :: (Ord a, DS.NFData b, DS.NFData a) =>
                    (a -> a -> b) -> [a] -> Map.Map (a, a) b
computeDistances distanceDefinition a = list2Map $ comD distanceDefinition a

--------------------------------------------
-- computeDistances1
--------------------------------------------
distanceBetween :: (Ord a, DS.NFData b, DS.NFData a) =>
                   (a -> a -> b) -> a -> [a] -> Map.Map (a, a) b
-- Bad implementation of `distanceBetween`
-- This takes a fairly amount of GC time, but I don't know why.
-- distanceBetween _ _ [] = Map.empty
-- distanceBetween distanceDefinition s (x:xs) =
--   P.runEval $ do
--     a <- P.rpar $ DS.force $ insert (s, x) (distanceDefinition s x) $ Map.empty
--     b <- P.rpar $ distanceBetween distanceDefinition s xs
--     P.rseq a
--     P.rseq b
--     return $ Map.union a b

distanceBetween distanceDefinition s a =
  foldr (\y -> insert (s, y) (distanceDefinition s y)) Map.empty a

computeDistances1 :: (Ord a, DS.NFData b, DS.NFData a) =>
                     (a -> a -> b) -> [a] -> Map.Map (a, a) b
computeDistances1 distanceDefinition (a:b:[]) =
  insert (a,b) (distanceDefinition a b) $ Map.empty
computeDistances1 distanceDefinition (x:xs) =
  P.runEval $ do
    a <- P.rpar $ DS.force $ distanceBetween distanceDefinition x xs
    b <- P.rpar $ computeDistances1 distanceDefinition xs
    P.rseq a
    P.rseq b
    return $ Map.union a b
