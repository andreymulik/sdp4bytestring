{-# LANGUAGE Trustworthy, MagicHash, MultiParamTypeClasses, CPP, FlexibleInstances #-}

{- |
    Module      :  SDP.ByteString
    Copyright   :  (c) Andrey Mulik 2019-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    "SDP.ByteString" provides @sdp@ instances for strict 'ByteString'.
-}
module SDP.ByteString
(
  -- * Exports
  module System.IO.Classes,
  
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  
  -- * ByteString
  ByteString, SByteString
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.SortM.Tim
import SDP.Indexed
import SDP.Sort
import SDP.Scan

import SDP.Prim.SBytes
import SDP.Bytes.ST

import Data.ByteString.Internal ( unsafeCreate )
import Data.ByteString          (  ByteString  )
import qualified Data.ByteString as B

import Data.Coerce
import Data.Maybe

import Foreign.Storable ( Storable ( poke ) )
import Foreign.Ptr      ( plusPtr )

import Control.Exception.SDP

import System.IO.Classes

default ()

--------------------------------------------------------------------------------

-- | Type synomym to avoid ambiguity.
type SByteString = ByteString

--------------------------------------------------------------------------------

{- Nullable, Forceable and Estimate instances. -}

instance Nullable ByteString
  where
    lzero  = B.empty
    isNull = B.null

#if MIN_VERSION_sdp(0,3,0)
instance Forceable ByteString where force = B.copy
#endif

instance Estimate ByteString
  where
    (<==>) = on (<=>) sizeOf
    (.<=.) = on (<=)  sizeOf
    (.>=.) = on (>=)  sizeOf
    (.>.)  = on (>)   sizeOf
    (.<.)  = on (<)   sizeOf
    
    (<.=>) = (<=>) . sizeOf
    (.>=)  = (>=)  . sizeOf
    (.<=)  = (<=)  . sizeOf
    (.>)   = (>)   . sizeOf
    (.<)   = (<)   . sizeOf

--------------------------------------------------------------------------------

{- Bordered, Linear and Split instances. -}

instance Bordered ByteString Int
  where
    lower      = const 0
    sizeOf     = B.length
    upper   bs = sizeOf bs - 1
    bounds  bs = (0, sizeOf bs - 1)
    indices bs = [0 .. sizeOf bs - 1]
    indexIn bs = \ i -> i >= 0 && i < sizeOf bs
#if MIN_VERSION_sdp(0,3,0)
    rebound    = B.take . size
#endif

instance Linear ByteString Word8
  where
    single = B.singleton
    toHead = B.cons
    toLast = B.snoc
    head   = B.head
    tail   = B.tail
    last   = B.last
    init   = B.init
    
    uncons' = B.uncons
    unsnoc' = B.unsnoc
    
    uncons = fromMaybe (pfailEx "uncons") . B.uncons
    unsnoc = fromMaybe (pfailEx "unsnoc") . B.unsnoc
    
    fromFoldable es = unsafeCreate (length es) fromFoldable'
      where
        fromFoldable' ptr = void $ foldr pokeNext (return ptr) es
        pokeNext  e   mp  = do p <- mp; poke p e; return $ p `plusPtr` 1
    
    listR = \ bs -> let n = sizeOf bs in [ bs .! i | i <- [n - 1, n - 2 .. 0] ]
    listL = B.unpack
    (++)  = B.append
    (!^)  = B.index
    
    write bs = (bs //) . single ... (,)
    
    concat      = B.concat . toList
    intersperse = B.intersperse
    replicate   = B.replicate
    filter      = B.filter
    fromList    = B.pack
    
    partitions is bs = map fromList . partitions is $ listL bs
    isSubseqOf xs ys = B.all (`B.elem` ys) xs
    
    nub bs = runST $ do
        hs <- filled 256 False
        o_foldr (\ b io -> writeM' hs b True >> io) (return ()) bs
        done' hs
      where
        done' :: STBytes s Word8 Bool -> ST s ByteString
        done' =  fmap fromList . kfoldrM (\ i b is -> return $ b ? (i : is) $ is) []
    
    nubBy f = fromList . o_foldr (\ b es -> any (f b) es ? es $ b : es) [] . nub
    
    ofoldr f = \ base bs ->
      let n = sizeOf bs; go i = n == i ? base $ f i (bs !^ i) (go $ i + 1)
      in  go 0
    
    ofoldl f = \ base bs ->
      let go i = -1 == i ? base $ f i (go $ i - 1) (bs !^ i)
      in  go (upper bs)
    
    o_foldr = B.foldr
    o_foldl = B.foldl
#if !MIN_VERSION_sdp(0,3,0)
instance Split ByteString Word8
  where
#endif
    take  = B.take
    drop  = B.drop
    split = B.splitAt
    
    isPrefixOf = B.isPrefixOf
    isSuffixOf = B.isSuffixOf
    isInfixOf  = B.isInfixOf
    takeWhile  = B.takeWhile
    dropWhile  = B.dropWhile
    
    spanl = B.span
    spanr = B.spanEnd
    
    breakl = B.break
    breakr = B.breakEnd

--------------------------------------------------------------------------------

{- Map and Indexed instances. -}

instance Map ByteString Int Word8
  where
    toMap = toMap' 0
    
    toMap' defvalue ascs = null ascs ? Z $ assoc' (l, u) defvalue ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    Z  // ascs = toMap ascs
    es // ascs = assoc (bounds es) (assocs es ++ ascs)
    
    (.!) = B.index
    (.$) = B.findIndex
    (*$) = B.findIndices
    
    kfoldr = ofoldr
    kfoldl = ofoldl

instance Indexed ByteString Int Word8
  where
    assoc = flip assoc' 0
    
    assoc' bnds defvalue ascs = unsafeCreate n fromAssocIO
      where
        fromAssocIO ptr = fill >> writeBS
          where
            writeBS = forM_ ies $  \ (i, e) -> poke (ptr `plusPtr` i) e
            fill = forM_ [0 .. n - 1] $ \ i -> poke (ptr `plusPtr` i) defvalue
        
        ies = [ (offset bnds i, e) | (i, e) <- ascs, inRange bnds i ]
        n   = size bnds
    
    fromIndexed es = let n = sizeOf es in unsafeCreate n $
        \ ptr -> forM_ [0 .. n - 1] $ \ i -> poke (ptr `plusPtr` i) (es !^ i)

--------------------------------------------------------------------------------

{- Sort and Scan instances. -}

-- TODO: write counting sort.
instance Sort ByteString Word8
  where
    sortBy f bs = runST $ do es' <- thaw bs; timSortBy f es'; done es'
    
    sortedBy f = sortedBy f . listL

instance Scan ByteString Word8

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance Thaw (ST s) ByteString (STBytes# s Word8) where thaw = fromIndexed'

instance Freeze (ST s) (STBytes# s Word8) ByteString where freeze = done

instance (MonadIO io) => Thaw io ByteString (MIOBytes# io Word8) where thaw = fromIndexed'

instance (MonadIO io) => Freeze io (MIOBytes# io Word8) ByteString where freeze = stToMIO . done . coerce

--------------------------------------------------------------------------------

{- IsFile and IsTextFile instances. -}

instance IsFile ByteString
  where
    hGetContents = liftIO  .  B.hGetContents
    hPutContents = liftIO ... B.hPut

instance IsTextFile ByteString
  where
    -- | Print bytestring and CR (0xa) character in Handle encoding.
    hPutStrLn h = liftIO  .  (>> hPutChar h '\n') . hPutStr h
    hGetLine    = liftIO  .  B.hGetLine
    hPutStr     = liftIO ... B.hPut

--------------------------------------------------------------------------------

done :: STBytes# s Word8 -> ST s ByteString
done =  fmap fromList . getLeft

pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.ByteString.Lazy."

