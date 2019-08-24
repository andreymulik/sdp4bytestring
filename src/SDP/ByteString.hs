{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, BangPatterns, MagicHash, UnboxedTuples #-}

{- |
    Module      :  SDP.ByteString
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    sdp4bytestring is SDP wrapper for ByteString.
    
    SDP.ByteString provides 'Bordered', 'Linear', 'Split', Indexed', 'Sort',
    'Thaw' and 'Freeze' instances for 'ByteString'.
    
    This wrapper is made for the convenience of using ByteString with other data
    structures - all the original functionality is available, some missing
    generalized functions have also been written.
    
    Unfortunately, Functor and Foldable for ByteString cannot be implemented,
    but I'm working on it.
-}
module SDP.ByteString
(
  module SDP.IndexedM,
  
  ByteString
)
where

import Prelude ()
import SDP.SafePrelude

-- SDP definitions

import SDP.IndexedM
import SDP.Unboxed
import SDP.Sort

-- ByteString definitions

import Data.ByteString          (  ByteString  )
import Data.ByteString.Internal ( unsafeCreate )
import qualified Data.ByteString as B

-- low-level definitions

import Foreign.Storable ( Storable ( poke ) )
import Foreign.Ptr      ( plusPtr )

import GHC.Base ( MutableByteArray#, Int (..), isTrue#, (==#), (+#), (-#) )
import GHC.ST   ( runST, ST (..), STRep )

-- common stuff

import Control.Exception.SDP

import SDP.Bytes.ST
import SDP.SortM.Stuff

default ()

--------------------------------------------------------------------------------

{- Bordered, Linear and Split instances. -}

instance Bordered ByteString Int Word8
  where
    lower _  = 0
    upper bs = B.length bs - 1
    sizeOf   = B.length

instance Linear ByteString Word8
  where
    single = B.singleton
    lzero  = B.empty
    isNull = B.null
    toHead = B.cons
    toLast = B.snoc
    head   = B.head
    tail   = B.tail
    last   = B.last
    init   = B.init
    
    fromFoldable es = unsafeCreate (length es) fromFoldable'
      where
        fromFoldable' ptr = void $ foldr pokeNext (return ptr) es
        pokeNext  e   mp  = do p <- mp; poke p e; return $ p `plusPtr` 1
    
    fromList = B.pack
    listL    = B.unpack
    (++)     = B.append
    
    listR   bs  = (bs .!) <$> [n - 1, n - 2 .. 0] where n = sizeOf bs
    concat  bss = B.concat $ toList bss
    intersperse = B.intersperse
    replicate   = B.replicate
    filter      = B.filter
    
    partitions is bs = map fromList . partitions is $ listL bs
    isSubseqOf xs ys = B.all (`B.elem` ys) xs
    
    -- | O(n) nub, requires O(1) memory.
    nub bs = runST $ do
      hs <- filled 256 False
      B.foldr (\ b io -> writeM hs (fromEnum b) True >> io) (return ()) bs
      toList' hs
    
    -- O(n) nubBy, requires O(1) additional memory.
    nubBy f bs = nubBy_ f (nub bs)

instance Split ByteString Word8
  where
    take  = B.take
    drop  = B.drop
    split = B.splitAt
    
    isPrefixOf = B.isPrefixOf
    isSuffixOf = B.isSuffixOf
    isInfixOf  = B.isInfixOf
    
    takeWhile = B.takeWhile
    dropWhile = B.dropWhile
    
    spanl = B.span
    spanr = B.spanEnd
    
    breakl = B.break
    breakr = B.breakEnd
    
    prefix p = B.foldr (\ e c -> p e ? c + 1 $ 0) 0
    suffix p = B.foldl (\ c e -> p e ? c + 1 $ 0) 0

--------------------------------------------------------------------------------

{- Indexed and Sort instances. -}

instance Indexed ByteString Int Word8
  where
    assoc  bnds ascs = assoc' bnds 0 ascs
    assoc' bnds defvalue ascs = unsafeCreate n fromAssocIO
      where
        fromAssocIO ptr = fill >> writeBS
          where
            writeBS = forM_ ies $  \ (i, e) -> poke (ptr `plusPtr` i) e
            fill = forM_ [0 .. n - 1] $ \ i -> poke (ptr `plusPtr` i) defvalue
        
        ies = [ (offset bnds i, e) | (i, e) <- ascs, inRange bnds i ]
        n   = size bnds
    
    (!^) = B.index
    (.!) = B.index
    (!)  = B.index
    
    Z  // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    es // ascs = assoc (bounds es) (assocs es ++ ascs)
    
    fromIndexed es = let n = sizeOf es in unsafeCreate n $
        \ ptr -> forM_ [0 .. n - 1] $ \ i -> poke (ptr `plusPtr` i) (es !^ i)
    
    (.$) = B.findIndex
    (*$) = B.findIndices

instance Sort ByteString Word8
  where
    sortBy f bs = runST $ do es <- thaw bs; timSortBy f es; freeze' es

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance Thaw (ST s) ByteString (STBytes s Int Word8)
  where
    thaw bs = ST $ \ s1# -> case newUnboxed e n# s1# of
      (# s2#, marr# #) ->
        let go y r = \ i# s3# -> case writeByteArray# marr# i# y s3# of
              s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
        in done n marr# ( if n == 0 then s2# else B.foldr go (\ _ s# -> s#) bs 0# s2# )
      where
        e = unreachEx "thaw" :: Word8
        !n@(I# n#) = sizeOf bs

instance Freeze (ST s) (STBytes s Int Word8) ByteString where freeze = freeze'

--------------------------------------------------------------------------------

cmpfst :: (Ord a) => (a, b) -> (a, b) -> Ordering
cmpfst (x, _) (y, _) = x <=> y

{-
  nubBy_ has complexity O(n ^ 2). However, when using it in nubBy n <= 256,
  and therefore the complexity of this step is O(1).
-}
nubBy_ :: (Word8 -> Word8 -> Bool) -> ByteString -> ByteString
nubBy_ f bs = fromList $ B.foldr (\ b es -> any (f b) es ? es $ (b : es)) [] bs

toList' :: STBytes s Int Bool -> ST s ByteString
toList' bytes = fromList <$> foldr go (return []) [0 .. 255]
  where
    go i = liftA2 (\ b is -> b ? (toEnum i : is) $ is) (bytes !> i)

done :: Int -> MutableByteArray# s -> STRep s (STBytes s Int Word8)
done n mbytes# = \ s1# -> (# s1#, STBytes 0 (n - 1) n mbytes# #)

freeze' :: STBytes s Int Word8 -> ST s ByteString
freeze' es = fromList <$> getLeft es

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in Data.ByteString.SDP." ++ msg



