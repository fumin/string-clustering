{-# LANGUAGE ForeignFunctionInterface #-}
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map as Map

-- parellel
import qualified Control.Parallel.Strategies as P

-- File IO
import System.Environment (getArgs)

import qualified Data.ByteString as B
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe
foreign import ccall unsafe "damerau_levenshtein.h damerau_levenshtein"
  c_damerau_levenshtein :: CString -> CString -> CInt

damerauLevenshtein :: LC.ByteString -> LC.ByteString -> Integer
damerauLevenshtein a b = unsafePerformIO $
  B.useAsCString (B.concat (LC.toChunks a)) $ \astr -> do
  B.useAsCString (B.concat (LC.toChunks b)) $ \bstr -> do
    return (fromIntegral (c_damerau_levenshtein astr bstr))


(!) :: Map.Map (LC.ByteString, LC.ByteString) Integer ->
       (LC.ByteString, LC.ByteString) ->
       Integer
(!) distances (a, b)
  | a >  b    = distances Map.! (a, b)
  | a == b    = 0
  | otherwise = distances ! (b, a)

insert :: (LC.ByteString, LC.ByteString) ->
          Integer ->
          Map.Map (LC.ByteString, LC.ByteString) Integer ->
          Map.Map (LC.ByteString, LC.ByteString) Integer
insert (a, b) v distances
  | a > b     = Map.insert (a, b) v distances
  | a == b    = distances
  | otherwise = insert (b, a) v distances


--dd =
--  Main.insert ((LC.pack "abc"), (LC.pack "cde")) 5 .
--  Main.insert ((LC.pack "abc"), (LC.pack "cq")) 4 .
--  Main.insert ((LC.pack "dfk"), (LC.pack "cde")) 50 $ Map.empty

computeDistances :: [LC.ByteString] -> Map.Map (LC.ByteString, LC.ByteString) Integer
computeDistances (a:b:[]) = Map.fromList [((a,b), (damerauLevenshtein a b))]
computeDistances (x:xs) = P.runEval $ do
                            a <- P.rpar $ Map.fromList $ P.parMap P.rseq (\y -> ((x, y), (damerauLevenshtein x y))) xs
                            b <- P.rpar $ computeDistances xs
                            P.rseq a
                            P.rseq b
                            return $ Map.union a b

main = do
  args <- getArgs
  contents <- LC.readFile (args !! 0)
  let dd = computeDistances $ LC.lines contents
  putStrLn $ show $ dd ! ((LC.pack "user:1183942:activity_feed"), (LC.pack "user:2328050:activity_feed"))
  putStrLn $ show $ dd ! ((LC.pack "user:1183942:activity_feed"), (LC.pack "collage:4577422:liked_by"))
  putStrLn $ show $ dd ! ((LC.pack "user:2328050:activity_feed"), (LC.pack "collage:4577422:liked_by"))
