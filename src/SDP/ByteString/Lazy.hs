{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE BangPatterns, MagicHash #-}

{- |
    Module      :  SDP.ByteString.Lazy
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    SDP.ByteString.Lazy provides 'Bordered', 'Linear', 'Split', 'Indexed',
    'Sort', 'Thaw' and 'Freeze' instances for 'ByteString'.
    
    This wrapper is made for the convenience of using ByteString with other data
    structures - all the original functionality is available, some missing
    generalized functions have also been written.
    
    Unfortunately, some functions from ByteString cannot be generalized
    (Functor, for example). This wrapper simplifies the work with the library,
    but some things are simply impossible to do.
-}
module SDP.ByteString.Lazy
(
  module SDP.IndexedM,
  
  ByteString
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Sort

import SDP.SortM.Tim

import SDP.ByteList.ST
import SDP.ByteList.STUblist

import Data.ByteString.Lazy.Internal ( ByteString (..) )
import qualified Data.ByteString.Lazy as B
import qualified SDP.ByteString as S

import GHC.ST ( runST, ST )

default ()

--------------------------------------------------------------------------------

{- Bordered, Linear and Split instances. -}

instance Bordered ByteString Int Word8
  where
    lower      = const 0
    sizeOf     = fromEnum . B.length
    upper   bs = sizeOf bs - 1
    bounds  bs = (0, sizeOf bs - 1)
    indices bs = [0 .. sizeOf bs - 1]
    indexIn bs = \ i -> i >= 0 && i < sizeOf bs

instance Linear ByteString Word8
  where
    replicate   = B.replicate . toEnum
    concat      = B.concat . toList
    intersperse = B.intersperse
    filter      = B.filter
    fromList    = B.pack
    
    listR  = \ bs -> let n = sizeOf bs in [ bs .! i | i <- [n - 1, n - 2 .. 0] ]
    single = B.singleton
    listL  = B.unpack
    (++)   = B.append
    lzero  = B.empty
    isNull = B.null
    toHead = B.cons
    toLast = B.snoc
    head   = B.head
    tail   = B.tail
    last   = B.last
    init   = B.init
    
    partitions is bs = map fromList . partitions is $ listL bs
    isSubseqOf xs ys = B.all (`B.elem` ys) xs
    
    -- | O(n) nub, requires O(1) memory.
    nub bs = runST $ do
        hs <- filled 256 False
        B.foldr (\ b io -> writeM hs b True >> io) (return ()) bs
        done' hs
      where
        done' :: STByteList s Word8 Bool -> ST s ByteString
        done' =  fmap fromList . ifoldrM (\ i b is -> pure $ b ? (i : is) $ is) []
    
    -- O(n) nubBy, requires O(1) memory.
    nubBy f = fromList . B.foldr (\ b es -> any (f b) es ? es $ (b : es)) [] . nub

instance Split ByteString Word8
  where
    take  = B.take    . toEnum
    drop  = B.drop    . toEnum
    split = B.splitAt . toEnum
    
    isPrefixOf = B.isPrefixOf
    isSuffixOf = B.isSuffixOf
    isInfixOf  = \ xs ys -> listL xs `isInfixOf` listL ys
    
    takeWhile = B.takeWhile
    dropWhile = B.dropWhile
    
    spanl  = B.span
    breakl = B.break
    
    prefix p = B.foldr (\ e c -> p e ? c + 1 $ 0) 0
    suffix p = B.foldl (\ c e -> p e ? c + 1 $ 0) 0

--------------------------------------------------------------------------------

{- Indexed and Sort instances. -}

instance Indexed ByteString Int Word8
  where
    assoc  bnds ascs = assoc' bnds 0 ascs
    assoc' bnds@(l, _) defvalue ascs = B.fromChunks $
        go l [ (offset bnds i, e) | (i, e) <- ascs, inRange bnds i ]
      where
        go :: Int -> [(Int, Word8)] -> [S.ByteString]
        go _   []  = []
        go cl ies' = chunk : go nl rest
          where
            chunk = fromList $ assoc' (cl, nl - 1) defvalue ch
            (ch, rest) = partition (\ (i, _) -> i < nl) ies'
            nl = cl + lim
    
    (!^) es = B.index es . toEnum
    (.!) es = B.index es . toEnum
    (!)  es = B.index es . toEnum
    
    Z  // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    es // ascs = assoc (bounds es) (assocs es ++ ascs)
    
    fromIndexed es = assoc bnds' [ (bnds `offset` i, e) | (i, e) <- assocs es ]
      where
        bnds' = defaultBounds (sizeOf es)
        bnds  = bounds es
    
    p .$ es = fromEnum <$> B.findIndex   p es
    p *$ es = fromEnum <$> B.findIndices p es

instance Sort ByteString Word8
  where
    sortBy f bs = runST $ do es' <- thaw bs; timSortBy f es'; done es'

--------------------------------------------------------------------------------

instance Thaw (ST s) ByteString (STUblist s Word8)
  where
    thaw Empty = return STUBEmpty
    thaw (Chunk str lazy) = liftA2 STUblist (S.thaw str) (thaw lazy)

instance Thaw (ST s) ByteString (STByteList s Int Word8)
  where
    thaw bs = STByteList 0 (-1) <$> thaw bs

instance Freeze (ST s) (STUblist s Word8) ByteString
  where
    freeze STUBEmpty = return Empty
    freeze (STUblist arr# ubl) = liftA2 Chunk (S.freeze arr#) (freeze ubl)

instance Freeze (ST s) (STByteList s Int Word8) ByteString
  where
    freeze (STByteList _ _ bs) = freeze bs

--------------------------------------------------------------------------------

instance Estimate ByteString
  where
    (<==>) = go 0
      where
        go o Empty Empty = o <=> 0
        go o xs    Empty = xs <.=> (-o)
        go o Empty    ys = o <=.> ys
        go o (Chunk str1 lazy1) (Chunk str2 lazy2) =
          let n1 = sizeOf str1; n2 = sizeOf str2
          in  go (o + n1 - n2) lazy1 lazy2
    
    Empty <.=> n = 0 <=> n
    (Chunk str lazy) <.=> m = str .> m ? GT $ lazy <.=> (m - sizeOf str)

--------------------------------------------------------------------------------

done :: STUblist s Word8 -> ST s ByteString
done = freeze

lim :: Int
lim =  1024

