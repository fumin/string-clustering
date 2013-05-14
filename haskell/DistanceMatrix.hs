{-# LANGUAGE ForeignFunctionInterface #-}
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map as Map
import qualified Data.Char as Char

-- ST
import qualified Control.Monad as M
import qualified Control.Monad.ST as ST
import qualified Data.STRef as STRef
import qualified Data.Array.ST as AST

-- parellel
import qualified Control.Parallel.Strategies as P
import qualified Control.DeepSeq as DS

-- File IO
import System.Environment (getArgs)

-- FFI
import qualified Data.ByteString as B
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe
foreign import ccall "damerau_levenshtein.h damerau_levenshtein"
  c_damerau_levenshtein :: CString -> CString -> CInt

damerauLevenshtein :: LC.ByteString -> LC.ByteString -> Integer
damerauLevenshtein a b = unsafePerformIO $
  B.useAsCString (B.concat (LC.toChunks a)) $ \astr -> do
  B.useAsCString (B.concat (LC.toChunks b)) $ \bstr -> do
    return (fromIntegral (c_damerau_levenshtein astr bstr))

-- On the first 500 keys of sampled
-- FFI -> 0.1s, STUArray -> 25s, STArray -> 30s, mainly spent of GC.
-- This validates the claim that unboxed is GC friendlier, too.
--damerauLevenshtein a b
--  | len_a >= max_len || len_b >= max_len = fromIntegral max_len
--  | len_a == 0 && len_b == 0 = 0
--  | len_a == 0 = fromIntegral len_b
--  | len_b == 0 = fromIntegral len_a
--  | otherwise = fromIntegral $ ST.runST $ do
--    -- prepare score
--    score <- AST.newArray ((0, 0), ((fromIntegral max_len)+1, (fromIntegral max_len)+1)) 0 :: ST.ST s (AST.STUArray s (Int, Int) Int)
--    M.forM_ [0..len_a] $ \i -> do
--      AST.writeArray score ((fromIntegral i)+1, 1) (fromIntegral i)
--      AST.writeArray score ((fromIntegral i)+1, 0) (fromIntegral inf)
--    M.forM_ [0..len_b] $ \j -> do
--      AST.writeArray score (1, (fromIntegral j)+1) (fromIntegral j)
--      AST.writeArray score (0, (fromIntegral j)+1) (fromIntegral inf)
--    -- prepare sd
--    sd <- AST.newArray (0, char_set_size-1) 0 :: ST.ST s (AST.STUArray s Int Int)
--    -- local variables
--    db <- STRef.newSTRef 0
--    i1 <- STRef.newSTRef 0
--    j1 <- STRef.newSTRef 0
--    M.forM_ [1..len_a] $ \i -> do
--      STRef.writeSTRef db 0
--      M.forM_ [1..len_b] $ \j -> do
--        STRef.writeSTRef i1 =<< (AST.readArray sd (Char.ord (b `LC.index` ((fromIntegral j)-1))))
--        STRef.writeSTRef j1 =<< (STRef.readSTRef db)
--        if a `LC.index` (i-1) == b `LC.index` (j-1) then do
--          AST.writeArray score ((fromIntegral i)+1, (fromIntegral j)+1) =<< (AST.readArray score ((fromIntegral i), (fromIntegral j)))
--          STRef.writeSTRef db j
--        else do
--          values <- mapM (AST.readArray score) [((fromIntegral i),(fromIntegral j)), ((fromIntegral i)+1,(fromIntegral j)), ((fromIntegral i),(fromIntegral j)+1)]
--          AST.writeArray score ((fromIntegral i)+1, (fromIntegral j)+1) (1 + (minimum values))
--
--        i1' <- STRef.readSTRef i1
--        j1' <- STRef.readSTRef j1
--        ip1jp1 <- (AST.readArray score ((fromIntegral i)+1,(fromIntegral j)+1))
--        i1j1 <- (AST.readArray score ((fromIntegral i1'), (fromIntegral j1')))
--        let value = min ip1jp1 (i1j1 + (fromIntegral i)-(fromIntegral i1') + (fromIntegral j)-(fromIntegral j1')-1)
--        AST.writeArray score ((fromIntegral i)+1, (fromIntegral j)+1) value
--
--      AST.writeArray sd (Char.ord (a `LC.index` (i-1))) (fromIntegral i)
--
--    AST.readArray score ((fromIntegral len_a)+1, (fromIntegral len_b)+1)
--  where len_a = LC.length a
--        len_b = LC.length b
--        max_len = 128
--        char_set_size = 256
--        inf = len_a + len_b


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

------------------------------------------------------------------
-- 1st computeDistance 60s
--distanceBetween :: LC.ByteString ->
--                   [LC.ByteString] ->
--                   Map.Map (LC.ByteString, LC.ByteString) Integer
--distanceBetween s [] = Map.empty
----distanceBetween s (x:xs) =
----  Map.union (insert (s, x) (damerauLevenshtein s x) $ Map.empty) (distanceBetween s xs)
----distanceBetween s (x:xs) = P.runEval $ do
----                             a <- P.rpar $ insert (s, x) (damerauLevenshtein s x) $ Map.empty
----                             b <- P.rpar $ distanceBetween s xs
----                             P.rseq a
----                             P.rseq b
----                             return $ Map.union a b
--distanceBetween s a = foldr (\y -> insert (s, y) (damerauLevenshtein s y)) Map.empty a
--computeDistances :: [LC.ByteString] -> Map.Map (LC.ByteString, LC.ByteString) Integer
--computeDistances (a:b:[]) = Map.fromList [((a,b), (damerauLevenshtein a b))]
--computeDistances (x:xs) = P.runEval $ do
--                            a <- P.rpar $ distanceBetween x xs
--                            b <- P.rpar $ computeDistances xs
--                            P.rseq a
--                            P.rseq b
--                            return $ Map.union a b

----------------------------------------------------
-- 2nd computeDistance 40s

-- This is EXTREMELY important, as it allows us to use P.rdeepseq
instance DS.NFData LC.ByteString where
  rnf a
    | LC.empty == a = ()
    | otherwise = DS.rnf $ LC.fromChunks $ tail $ LC.toChunks a

comD :: [LC.ByteString] -> [((LC.ByteString, LC.ByteString), Integer)]
comD a = concat $ P.parMap P.rdeepseq (\n -> subDist n a) [1..(length a)]
subDist :: Int -> [LC.ByteString] -> [((LC.ByteString, LC.ByteString), Integer)]
subDist n a = map (\s -> ((x, s), (damerauLevenshtein x s))) bs
  where (as,bs) = splitAt n a
        x = last as
--comD (a:b:[]) = [((a, b), (damerauLevenshtein a b))]
--comD (x:xs) = P.runEval $ do
--                a <- P.rpar $ P.parMap P.rseq (\y -> ((x, y), (damerauLevenshtein x y))) xs
--                b <- P.rpar $ comD xs
--                P.rseq a
--                P.rseq b
--                return $ a ++ b
list2Map :: [((LC.ByteString, LC.ByteString), Integer)] -> Map.Map (LC.ByteString, LC.ByteString) Integer
list2Map a
  | 100 > (length a) = foldr (\t -> insert (fst t) (snd t)) Map.empty a
  | otherwise = P.runEval $ do
                  as' <- P.rpar $ list2Map as
                  bs' <- P.rpar $ list2Map bs
                  P.rseq as'
                  P.rseq bs'
                  return $ Map.union as' bs'
  where (as,bs) = splitAt ((length a) `div` 2) a
computeDistances :: [LC.ByteString] -> Map.Map (LC.ByteString, LC.ByteString) Integer
computeDistances a = list2Map $ comD a

main = do
  args <- getArgs
  strings <- fmap LC.lines $ LC.readFile (args !! 0)
  let dd = computeDistances strings
  putStrLn $ show $ Map.size dd
  putStrLn $ show $ Map.foldr (\v sum -> v + sum) 0 dd
--  putStrLn $ show $ dd ! ((LC.pack "user:1183942:activity_feed"), (LC.pack "user:2328050:activity_feed"))
--  putStrLn $ show $ dd ! ((LC.pack "user:1183942:activity_feed"), (LC.pack "collage:4577422:liked_by"))
--  putStrLn $ show $ dd ! ((LC.pack "user:2328050:activity_feed"), (LC.pack "collage:4577422:liked_by"))
