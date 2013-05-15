{-# LANGUAGE ForeignFunctionInterface #-}
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map as Map
import qualified Control.DeepSeq as DS
import System.Environment (getArgs)

import qualified StringDistance as SD
import qualified ComputeDistances as CD

damerauLevenshtein :: LC.ByteString -> LC.ByteString -> Integer
damerauLevenshtein a b = SD.damerauLevenshtein a b

computeDistances :: [LC.ByteString] -> Map.Map (LC.ByteString, LC.ByteString) Integer
computeDistances a = CD.computeDistances damerauLevenshtein a

(!) :: Map.Map (LC.ByteString, LC.ByteString) Integer ->
       (LC.ByteString, LC.ByteString) ->
       Integer
(!) distances (a, b) = distances CD.! (a, b)

insert :: (LC.ByteString, LC.ByteString) ->
          Integer ->
          Map.Map (LC.ByteString, LC.ByteString) Integer ->
          Map.Map (LC.ByteString, LC.ByteString) Integer
insert (a, b) v distances = CD.insert (a, b) v distances

--dd =
--  Main.insert ((LC.pack "abc"), (LC.pack "cde")) 5 .
--  Main.insert ((LC.pack "abc"), (LC.pack "cq")) 4 .
--  Main.insert ((LC.pack "dfk"), (LC.pack "cde")) 50 $ Map.empty

-- This is EXTREMELY important, as it allows us to use P.rdeepseq
instance DS.NFData LC.ByteString where
  rnf a
    | LC.empty == a = ()
    | otherwise = DS.rnf $ LC.fromChunks $ tail $ LC.toChunks a

main = do
  args <- getArgs
  strings <- fmap LC.lines $ LC.readFile (args !! 0)
  let dd = computeDistances strings
  putStrLn $ show $ Map.size dd
  putStrLn $ show $ Map.foldr (\v sum -> v + sum) 0 dd
--  putStrLn $ show $ dd ! ((LC.pack "user:1183942:activity_feed"), (LC.pack "user:2328050:activity_feed"))
--  putStrLn $ show $ dd ! ((LC.pack "user:1183942:activity_feed"), (LC.pack "collage:4577422:liked_by"))
--  putStrLn $ show $ dd ! ((LC.pack "user:2328050:activity_feed"), (LC.pack "collage:4577422:liked_by"))
