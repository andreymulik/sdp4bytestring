{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, BangPatterns, MagicHash, UnboxedTuples #-}

{- |
    Module      :  SDP.ByteString
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    SDP.ByteString provides 'Bordered', 'Linear', 'Split', Indexed', 'Sort',
    'Estimate', 'Thaw' and 'Freeze' instances for 'ByteString'.
    
    This wrapper is made for the convenience of using ByteString with other data
    structures - all the original functionality is available, some missing
    generalized functions have also been written.
    
    Unfortunately, some functions from ByteString cannot be generalized
    (Functor, for example). This wrapper simplifies the work with the library,
    but some things are simply impossible to do.
-}
module SDP.ByteString
(
  module SDP.IndexedM,
  module SDP.Sort,
  
  ByteString
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Sort

import Data.ByteString                (  ByteString  )
import Data.ByteString.Internal       ( unsafeCreate )
import qualified Data.ByteString as B

import Foreign.Storable ( Storable ( poke ) )
import Foreign.Ptr      (      plusPtr      )

import GHC.Base (    Int (..)    )
import GHC.ST   ( runST, ST (..) )

import SDP.Bytes.ST
import SDP.SortM.Tim

default ()

--------------------------------------------------------------------------------

{- Bordered, Linear and Split instances. -}

instance Bordered ByteString Int Word8
  where
    lower      = const 0
    sizeOf     = B.length
    upper   bs = sizeOf bs - 1
    bounds  bs = (0, sizeOf bs - 1)
    indices bs = [0 .. sizeOf bs - 1]
    indexIn bs = \ i -> i >= 0 && i < sizeOf bs

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
    
    listR = \ bs -> let n = sizeOf bs in [ bs .! i | i <- [n - 1, n - 2 .. 0] ]
    listL = B.unpack
    (++)  = B.append
    
    concat      = B.concat . toList
    intersperse = B.intersperse
    replicate   = B.replicate
    filter      = B.filter
    fromList    = B.pack
    
    partitions is bs = map fromList . partitions is $ listL bs
    isSubseqOf xs ys = B.all (`B.elem` ys) xs
    
    -- | O(n) nub, requires O(1) memory.
    nub bs = runST $ do
        hs <- filled 256 False
        i_foldr (\ b io -> writeM hs b True >> io) (return ()) bs
        done' hs
      where
        done' :: STBytes s Word8 Bool -> ST s ByteString
        done' =  fmap fromList . ifoldrM (\ i b is -> pure $ b ? (i : is) $ is) []
    
    -- O(n) nubBy, requires O(1) additional memory.
    nubBy f = fromList . i_foldr (\ b es -> any (f b) es ? es $ (b : es)) [] . nub

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
    sortBy f bs = runST $ do es' <- thaw bs; timSortBy f es'; done es'

--------------------------------------------------------------------------------

{- IFold and Estimate instances. -}

instance IFold ByteString Int Word8
  where
    {-# INLINE ifoldr #-}
    ifoldr f = \ base bs ->
      let go i = sizeOf bs == i ? base $ f i (bs !^ i) (go $ i + 1)
      in  go 0
    
    {-# INLINE ifoldl #-}
    ifoldl f = \ base bs ->
      let go i = -1 == i ? base $ f i (go $ i + 1) (bs !^ i)
      in  go (upper bs)
    
    i_foldr = B.foldr
    i_foldl = B.foldl

instance Estimate ByteString
  where
    xs <==> ys = sizeOf xs <=> sizeOf ys
    xs .>.  ys = sizeOf xs  >  sizeOf ys
    xs .<.  ys = sizeOf xs  <  sizeOf ys
    xs .<=. ys = sizeOf xs <=  sizeOf ys
    xs .>=. ys = sizeOf xs >=  sizeOf ys
    xs <.=> c2 = sizeOf xs <=> c2
    xs  .>  c2 = sizeOf xs  >  c2
    xs  .<  c2 = sizeOf xs  <  c2
    xs .>=  c2 = sizeOf xs >=  c2
    xs .<=  c2 = sizeOf xs <=  c2

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance Thaw (ST s) ByteString (STBytes# s Word8)    where thaw = fromIndexed'
instance Thaw (ST s) ByteString (STBytes s Int Word8) where thaw = fromIndexed'

instance Freeze (ST s) (STBytes# s Word8) ByteString
  where
    freeze = fmap fromList . getLeft

instance Freeze (ST s) (STBytes s Int Word8) ByteString where freeze = done

--------------------------------------------------------------------------------

done :: STBytes s Int Word8 -> ST s ByteString
done =  fmap fromList . getLeft

