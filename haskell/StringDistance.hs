{-# LANGUAGE ForeignFunctionInterface #-}

-- The experiment computes the distances between all pairs of a set of 500 strings,
-- which is equivalent of calling the function 124750 times.
-- We compute them in parallel by:
--
-- comD :: [LC.ByteString] -> [((LC.ByteString, LC.ByteString), Integer)]
-- comD a = concat $ P.parMap P.rdeepseq (\n -> subDist n a) [1..(length a)]
-- subDist :: Int -> [LC.ByteString] -> [((LC.ByteString, LC.ByteString), Integer)]
-- subDist n a = map (\s -> ((x, s), (damerauLevenshtein x s))) bs
--   where (as,bs) = splitAt n a
--         x = last as
--
-- The results are:
-- | Implementation                 | Time took (seconds)
-- | damerauLevenshtein (FFI)       | 0.5
-- | damerauLevenshtein1 (STUArray) | 25
-- | damerauLevenshtein1 (STArray)  | 30
-- The reason why the ST monad based damerauLevenshtein1 implementations
-- are so slow is because of heavy GC, around 85% of the time spent.
-- This experiment also validates the claim that
-- "unboxed" objects are a bit GC friendlier.

module StringDistance (damerauLevenshtein,
                       damerauLevenshtein1) where

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString as B
import qualified Data.Char as Char

-- ST
import qualified Control.Monad as M
import qualified Control.Monad.ST as ST
import qualified Data.STRef as STRef
import qualified Data.Array.ST as AST

-- FFI
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe

-- damerauLevenshtein, using FFI
foreign import ccall "damerau_levenshtein.h damerau_levenshtein"
  c_damerau_levenshtein :: CString -> CString -> CInt
damerauLevenshtein :: LC.ByteString -> LC.ByteString -> Integer
damerauLevenshtein a b = unsafePerformIO $
  B.useAsCString (B.concat (LC.toChunks a)) $ \astr -> do
  B.useAsCString (B.concat (LC.toChunks b)) $ \bstr -> do
    return (fromIntegral (c_damerau_levenshtein astr bstr))

-- damerauLevenshtein1, using the ST monad
damerauLevenshtein1 a b
  | len_a >= max_len || len_b >= max_len = fromIntegral max_len
  | len_a == 0 && len_b == 0 = 0
  | len_a == 0 = fromIntegral len_b
  | len_b == 0 = fromIntegral len_a
  | otherwise = fromIntegral $ ST.runST $ do
    -- prepare score
    score <- AST.newArray ((0, 0), ((fromIntegral max_len)+1, (fromIntegral max_len)+1)) 0 :: ST.ST s (AST.STUArray s (Int, Int) Int)
    M.forM_ [0..len_a] $ \i -> do
      AST.writeArray score ((fromIntegral i)+1, 1) (fromIntegral i)
      AST.writeArray score ((fromIntegral i)+1, 0) (fromIntegral inf)
    M.forM_ [0..len_b] $ \j -> do
      AST.writeArray score (1, (fromIntegral j)+1) (fromIntegral j)
      AST.writeArray score (0, (fromIntegral j)+1) (fromIntegral inf)
    -- prepare sd
    sd <- AST.newArray (0, char_set_size-1) 0 :: ST.ST s (AST.STUArray s Int Int)
    -- local variables
    db <- STRef.newSTRef 0
    i1 <- STRef.newSTRef 0
    j1 <- STRef.newSTRef 0
    M.forM_ [1..len_a] $ \i -> do
      STRef.writeSTRef db 0
      M.forM_ [1..len_b] $ \j -> do
        STRef.writeSTRef i1 =<< (AST.readArray sd (Char.ord (b `LC.index` ((fromIntegral j)-1))))
        STRef.writeSTRef j1 =<< (STRef.readSTRef db)
        if a `LC.index` (i-1) == b `LC.index` (j-1) then do
          AST.writeArray score ((fromIntegral i)+1, (fromIntegral j)+1) =<< (AST.readArray score ((fromIntegral i), (fromIntegral j)))
          STRef.writeSTRef db j
        else do
          values <- mapM (AST.readArray score) [((fromIntegral i),(fromIntegral j)), ((fromIntegral i)+1,(fromIntegral j)), ((fromIntegral i),(fromIntegral j)+1)]
          AST.writeArray score ((fromIntegral i)+1, (fromIntegral j)+1) (1 + (minimum values))

        i1' <- STRef.readSTRef i1
        j1' <- STRef.readSTRef j1
        ip1jp1 <- (AST.readArray score ((fromIntegral i)+1,(fromIntegral j)+1))
        i1j1 <- (AST.readArray score ((fromIntegral i1'), (fromIntegral j1')))
        let value = min ip1jp1 (i1j1 + (fromIntegral i)-(fromIntegral i1') + (fromIntegral j)-(fromIntegral j1')-1)
        AST.writeArray score ((fromIntegral i)+1, (fromIntegral j)+1) value

      AST.writeArray sd (Char.ord (a `LC.index` (i-1))) (fromIntegral i)

    AST.readArray score ((fromIntegral len_a)+1, (fromIntegral len_b)+1)
  where len_a = LC.length a
        len_b = LC.length b
        max_len = 128
        char_set_size = 256
        inf = len_a + len_b
