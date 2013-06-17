{-# LANGUAGE ForeignFunctionInterface #-}
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map as Map
import qualified Control.DeepSeq as DS
import qualified System.Random as Random
import System.Environment (getArgs)
import System.Exit (exitWith, exitFailure)

import qualified StringDistance as SD
import qualified ComputeDistances as CD
import qualified EM as EM

import Control.Exception

import qualified Debug.Trace as DT

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

parseArgs :: [[Char]] -> IO ([Char], Int)
parseArgs args =
  if length args /= 2
    then do putStrLn "Usage: string_clustering filename::String k::Int"
            exitFailure
    else return (args !! 0, read (args !! 1))

main = do
  args <- getArgs
  (filename, k) <- parseArgs args
  strings <- fmap LC.lines $ LC.readFile filename
  gen <- Random.getStdGen
  let distanceMap  = computeDistances strings
  evaluate distanceMap

  let sample_count = k * 3
      state'       = EM.em_restarts sample_count k strings distanceMap gen

  putStrLn "-----------------------------"
  putStrLn $ unlines $ map (LC.unpack . fst) (EM.classification state')
  putStrLn $ "variance: " ++ (show $ EM.variance state')
  putStrLn $ "in " ++ (show (EM.iteration state')) ++ " iterations"
