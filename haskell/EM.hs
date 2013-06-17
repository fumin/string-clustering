module EM (EMState(..),
           Status(..),
           initEMState,
           e,
           m,
           em,
           em_restarts) where

import qualified Data.List.Extras.Argmax as AR
import qualified Data.Maybe as M
import qualified Data.List as L
import qualified Data.List.Utils as LU
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified System.Random as Random

import qualified Control.Parallel.Strategies as P
import qualified Control.DeepSeq as DS

import qualified ComputeDistances as CD
import qualified Utils as Utils

import qualified Control.Monad.Par as PM
import qualified Control.Monad as CM

(!) :: (Num b, Ord a) => Map.Map (a, a) b -> (a, a) -> b
(!) distanceMap pair = distanceMap CD.! pair

data Status = Converged | Diverged deriving (Show, Eq)
instance DS.NFData Status where rnf a = ()

-- association list (centroid, points of this cluster)
type Classification a = [(a, [a])]

mergeClassification :: (Eq a) =>
                       Classification a -> Classification a -> Classification a
mergeClassification a b = F.foldr' mergeToClassification b a
  where mergeToClassification (centroid, points) b
          | pointsOfCentroidInB == Nothing = (centroid, points):b
          | otherwise = LU.addToAL b
                                   centroid 
                                   ((M.fromJust pointsOfCentroidInB) ++ points)
          where pointsOfCentroidInB = lookup centroid b

data EMState a b = EMState {
  -- inputs
  points         :: [a],
  distanceMap    :: Map.Map (a, a) b,
  -- outputs
  classification :: Classification a,
  variance       :: b,
  status         :: Status,
  iteration      :: Int
  } deriving (Show)

instance (DS.NFData a, DS.NFData b) => DS.NFData (EMState a b) where
  rnf (EMState points distanceMap classification variance status iteration) = DS.rnf variance `seq` DS.rnf status `seq` DS.rnf iteration

rnd_select :: (Random.RandomGen g) => Int -> [a] -> g -> [a]
rnd_select n x g = map (x!!) is -- we can test g with say (mkStdGen 100)
 where is = take n . L.nub $ Random.randomRs (0, length x - 1) g

initEMState :: (Num b, Ord b, Ord a, DS.NFData a, DS.NFData b, Random.RandomGen g) =>
               Int -> [a] -> Map.Map (a, a) b -> g -> EMState a b
initEMState k points distanceMap g =
  e $ EMState points distanceMap classification 0 Diverged 0
  where classification = (map (\c -> (c, [])) centroids)
        centroids = rnd_select k points g

--------------------------------------------
-- Expectation step of EM
--------------------------------------------
e :: (Ord b, Ord a, Eq a, Num b, DS.NFData a, DS.NFData b) =>
     EMState a b -> EMState a b
e state =
  EMState points' distanceMap' classification' variance' status' (iteration state)
  where points' = points state
        distanceMap' = distanceMap state
        centroids = LU.keysAL (classification state)
        (classification', variance') = classify points' centroids distanceMap'
        status' = status state

classify :: (Ord a, Eq a, Ord b, Num b, DS.NFData a, DS.NFData b) =>
            [a] -> [a] -> Map.Map (a, a) b -> (Classification a, b)
classify points centroids distanceMap =
  F.foldr' (\as' bs' -> ((mergeClassification (fst as') (fst bs')),
                         (snd as') + (snd bs')))
           ([], 0)
           (P.parMap P.rdeepseq classificationAndDistances
                                (Utils.splitEvery granularity points))
  where classificationAndDistances =
          F.foldr' addPointToClass ((map (\c -> (c, [])) centroids), 0) 
        addPointToClass p (classification, var)
          | members == Nothing =
              (LU.addToAL classification centroid [p],
               var + d)     
          | otherwise =
              (LU.addToAL classification centroid (p:(M.fromJust members)),
               var + d)
          where members = lookup centroid classification
                d       = distanceMap ! (p, centroid)
                centroid = AR.argmin (\c -> distanceMap ! (c, p)) centroids
                centroids = LU.keysAL classification
        granularity = 1000

--------------------------------------------
-- Maximization step of EM
--------------------------------------------
m :: (Ord a, Num b, Ord b, DS.NFData a, DS.NFData b) => EMState a b -> EMState a b
m state =
  EMState points' distanceMap' classification' variance' status' (iteration state)
  where points' = points state
        distanceMap' = distanceMap state
        classification' = (map (\c -> (c, [])) centroids')
        centroids' = setCentroids points' (classification state) distanceMap'
        variance' = variance state
        status' = status state

setCentroids :: (Ord a, Num b, Ord b, DS.NFData b, DS.NFData a) =>
                [a] -> Classification a -> Map.Map (a, a) b -> [a]
setCentroids points classification distanceMap =
  P.parMap P.rdeepseq
           (\cluster -> fst $ (newCentroid cluster points distanceMap))
           classification

-------------------------------------
-- 4 different implementations of `newCentroid`
-- The 4th one based on parMap is the best
-------------------------------------
-- 1st implementation: Eval monad divide and conquer
-- The problem with this implementation is that it creates too many
-- GC'd sparks compared to the parMap based implementation
--
--newCentroid (centroid, members) points distanceMap
--  | granularity > (length points) = AR.argminWithMin (distanceTo members distanceMap) points
--  | otherwise = P.runEval $ do
--      as' <- P.rpar $ DS.force $ newCentroid (centroid, members) as distanceMap
--      bs' <- P.rpar $ DS.force $ newCentroid (centroid, members) bs distanceMap 
--      P.rseq as'
--      P.rseq bs'
--      return $ AR.argmin snd [as', bs']
--  where granularity = 100
--        (as,bs) = splitAt (length points `div` 2) points
--        distanceTo members distanceMap p =
--          F.foldr' (\m sum -> (distanceMap ! (m, p)) + sum) 0 members

-- 2nd implementation: Wrong implementation based on par monad
-- The problem is that this implementation recursively calls `runPar`
-- which is a very expensive operation that involves
-- * waiting for all its subtasks to finish before returning (necessary for determinism)
-- * fires up a new gang of N threads and creates scheduling data structures: it's much more expensive than runEval
--
--newCentroid (centroid, members) points distanceMap
--  | granularity > (length points) = AR.argminWithMin (distanceTo members distanceMap) points
--  | otherwise = PM.runPar $ do
--      [i1, i2, ret_val] <- CM.replicateM 3 PM.new
--      let (as,bs) = splitAt (length points `div` 2) points
--      PM.fork $ PM.put i1 $ newCentroid (centroid, members) as distanceMap
--      PM.fork $ PM.put i2 $ newCentroid (centroid, members) bs distanceMap
--      PM.fork $ do as' <- PM.get i1
--                   bs' <- PM.get i2
--                   PM.put ret_val $ AR.argmin snd [as', bs']
--      ret_v <- PM.get ret_val
--      return ret_v
--  where granularity = 100
--        distanceTo members distanceMap p =
--          F.foldr' (\m sum -> (distanceMap ! (m, p)) + sum) 0 members

-- 3rd implementation: Correct implementation based on par monad
-- Still considerably slow compared to the parMap based implementation
--
--newCentroid :: (Num b, Ord a, Ord b, PM.NFData a, PM.NFData b) =>
--               (a, [a]) -> [a] -> Map.Map (a, a) b -> (a, b)
--newCentroid (centroid, members) points distanceMap =
--  PM.runPar $ parNewCentroid (centroid, members) points distanceMap
--parNewCentroid :: (Num b, Ord a, Ord b, PM.NFData a, PM.NFData b) =>
--                  (a, [a]) -> [a] -> Map.Map (a, a) b -> PM.Par (a, b)
--parNewCentroid (centroid, members) points distanceMap
--  | granularity > (length points) = return $ AR.argminWithMin (distanceTo members distanceMap) points
--  | otherwise = do
--      i1 <- PM.spawn $ parNewCentroid (centroid, members) as distanceMap
--      i2 <- PM.spawn $ parNewCentroid (centroid, members) bs distanceMap
--      as' <- PM.get i1
--      bs' <- PM.get i2
--      ret_val <- PM.spawn $ return $ AR.argmin snd [as', bs']
--      ret_v <- PM.get ret_val
--      return ret_v
--  where granularity = 100
--        distanceTo members distanceMap p =
--          F.foldr' (\m sum -> (distanceMap ! (m, p)) + sum) 0 members
--        (as,bs) = splitAt (length points `div` 2) points

-- 4th implementation: best implementation based on parMap
--
newCentroid (centroid, members) points distanceMap =
  AR.argmin snd (map--(P.parMap P.rdeepseq
                          (\ps -> AR.argminWithMin 
                                  (distanceTo members distanceMap)
                                  ps)
                          (Utils.splitEvery granularity points))
  where granularity = 100
        distanceTo members distanceMap p =
          F.foldr' (\m sum -> (distanceMap ! (m, p)) + sum) 0 members

em :: (Ord a, Num b, Ord b, DS.NFData a, DS.NFData b) =>
      EMState a b -> EMState a b
em s
  | i > 100 = s
  | (EM.variance s') == (EM.variance s) = convergedState
  | otherwise = em nextIterationState
  where convergedState =
          EMState points' distanceMap' classification' variance' Converged i
        nextIterationState =
          EMState points' distanceMap' classification' variance' Diverged (i+1)
        points'         = points s'
        distanceMap'    = distanceMap s'
        classification' = classification s'
        variance'       = variance s'
        i               = iteration s'
        s' = e $ m s

em_restarts :: (Ord a, Num b, Ord b, DS.NFData a, DS.NFData b, Random.RandomGen g) =>
               Int -> Int -> [a] -> Map.Map (a, a) b -> g -> EMState a b
em_restarts n k dataPoints distMap g =
  --AR.argmin variance ((EMState pp dist cll vv Converged i):states)
  AR.argmin variance states
  where states = P.parMap P.rdeepseq runEM nRandomNumbers
        runEM = em . initEMState k dataPoints distMap . Random.mkStdGen
        nRandomNumbers = take n $ Random.randomRs (1,1000000) g

        -- Below is for experimenting
        ss = states !! 0
        pp = points ss
        dist = distanceMap ss
        cll = classification ss
        vv = (sum $ P.parMap P.rdeepseq variance states) - 1
        i = iteration ss
