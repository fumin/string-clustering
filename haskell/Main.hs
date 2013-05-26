{-# LANGUAGE ForeignFunctionInterface #-}
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map as Map
import qualified Control.DeepSeq as DS
import qualified System.Random as Random
import System.Environment (getArgs)

import qualified StringDistance as SD
import qualified ComputeDistances as CD
import qualified EM as EM

damerauLevenshtein :: LC.ByteString -> LC.ByteString -> Integer
damerauLevenshtein a b = SD.damerauLevenshtein a b

computeDistances :: [LC.ByteString] -> Map.Map (LC.ByteString, LC.ByteString) Integer
computeDistances a = CD.computeDistances damerauLevenshtein a

(!) :: Map.Map (LC.ByteString, LC.ByteString) Integer ->
       (LC.ByteString, LC.ByteString) ->
       Integer
(!) distances (a, b) = distances CD.! (a, b)

instance DS.NFData LC.ByteString where
  rnf a
    | LC.empty == a = ()
    | otherwise = DS.rnf $ LC.fromChunks $ tail $ LC.toChunks a

main = do
  args <- getArgs
  strings <- fmap LC.lines $ LC.readFile (args !! 0)
  gen <- Random.getStdGen
  let distanceMap  = computeDistances strings
      k            = read (args !! 1)
      sample_count = k * 3
      state'       = EM.em_restarts sample_count k strings distanceMap gen
  putStrLn "-----------------------------"
  putStrLn $ unlines $ map (LC.unpack . fst) (EM.classification state')
  putStrLn $ "variance: " ++ (show $ EM.variance state')
  putStrLn $ "in " ++ (show (EM.iteration state')) ++ " iterations"
