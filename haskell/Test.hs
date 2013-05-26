import qualified Data.Maybe as MB
import qualified Test.QuickCheck as QC
import qualified System.Random as Random
import qualified Data.List.Extras.Argmax as AR
import qualified Data.List.Utils as LU
import qualified Data.Set as Set

import qualified ComputeDistances as CD
import qualified EM as EM

euclidean :: (Integral a, Floating b) => (a, a) -> (a, a) -> b
euclidean (a, b) (c, d) = sqrt $ ((a'-c')**2) + ((b'-d')**2)
  where a' = fromIntegral a
        b' = fromIntegral b
        c' = fromIntegral c
        d' = fromIntegral d

newtype EMState = EMState (EM.EMState (Int, Int) Float) deriving (Show)

instance QC.Arbitrary EMState where
  arbitrary = do let n = 36
                 points' <- QC.vectorOf n $ QC.arbitrary
                 k       <- QC.choose (2,  n `div` 3)
                 r       <- QC.choose (1, 10000)
                 let distanceMap = CD.computeDistances euclidean points'
                     g = Random.mkStdGen r
                 return $ EMState (EM.initEMState k points' distanceMap g)

-------------------------------------------
-- Check EM.e
-------------------------------------------
classPoints_and_variance :: EMState -> Bool
classPoints_and_variance (EMState state) =
  points == classPoints' && variance' < (1/0)
  where variance' = EM.variance state
        points = Set.fromList $ EM.points state
        classPoints' = toClassPoints state
        toClassPoints = Set.fromList . concat . LU.valuesAL . EM.classification

correct_classification :: EMState -> Bool
correct_classification (EMState state) = all isCorrectClassification points
  where points = EM.points state
        isCorrectClassification p =
          any (\k -> p `elem` (MB.fromJust $ lookup k classification')) klasses'
          where klasses' = AR.argmins (\c -> distanceMap CD.! (c, p)) centroids'
                centroids' = LU.keysAL classification'
                classification' = EM.classification state
                distanceMap = EM.distanceMap state

e :: EMState -> Bool
e s = (correct_classification s) && (classPoints_and_variance s)

-------------------------------------------
-- Check EM.em
-------------------------------------------
em :: EMState -> Bool
em (EMState state) =
  variance' <= variance && EM.Converged == (EM.status state')
  where variance' = EM.variance state'
        variance  = EM.variance state
        state'    = EM.em state

main = do
  QC.quickCheck e
  QC.quickCheck em
