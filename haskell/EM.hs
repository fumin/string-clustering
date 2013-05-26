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
classify points centroids distanceMap
  | granularity > (length points) = classificationAndDistances
  | otherwise = P.runEval $ do
                  as' <- P.rpar $ DS.force $ classify as centroids distanceMap
                  bs' <- P.rpar $ DS.force $ classify bs centroids distanceMap
                  P.rseq as'
                  P.rseq bs'
                  return $ ((mergeClassification (fst as') (fst bs')),
                            (snd as') + (snd bs'))
  where classificationAndDistances =
          F.foldr' addPointToClass ((map (\c -> (c, [])) centroids), 0) points
        addPointToClass p (classification, var)
          | members == Nothing = (LU.addToAL classification centroid [p], var + d)
          | otherwise = (LU.addToAL classification centroid (p:(M.fromJust members)), var + d)
          where members = lookup centroid classification
                d       = distanceMap ! (p, centroid)
                centroid = AR.argmin (\c -> distanceMap ! (c, p)) centroids
                centroids = LU.keysAL classification
        (as,bs) = splitAt ((length points) `div` 2) points
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

newCentroid (centroid, members) points distanceMap
  | granularity > (length points) =
      AR.argminWithMin (distanceTo members distanceMap) points
  | otherwise = P.runEval $ do
      as' <- P.rpar $ DS.force $ newCentroid (centroid, members) as distanceMap
      bs' <- P.rpar $ DS.force $ newCentroid (centroid, members) bs distanceMap
      P.rseq as'
      P.rseq bs'
      return $ AR.argmin snd [as', bs']
  where (as,bs) = splitAt ((length points) `div` 2) points
        distanceTo members distanceMap p =
          F.foldr' (\m sum -> (distanceMap ! (m, p)) + sum) 0 members
        granularity = 1000

em :: (Ord a, Num b, Ord b, DS.NFData a, DS.NFData b) => EMState a b -> EMState a b
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
em_restarts n k points distanceMap g =
  AR.argmin variance states
  where states = P.parMap P.rdeepseq runEM nRandomNumbers
        runEM = em . initEMState k points distanceMap . Random.mkStdGen
        nRandomNumbers = take n $ Random.randomRs (1,1000000) g
