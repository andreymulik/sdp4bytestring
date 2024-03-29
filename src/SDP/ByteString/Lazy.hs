{-# LANGUAGE Trustworthy, MagicHash, CPP, MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  SDP.ByteString.Lazy
    Copyright   :  (c) Andrey Mulik 2019-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    "SDP.ByteString.Lazy" provides @sdp@ instances for lazy 'ByteString'.
-}
module SDP.ByteString.Lazy
(
  -- * Exports
  module System.IO.Classes,
  
  module SDP.Indexed,
  module SDP.Sort,
  
  -- * ByteString
  ByteString, LByteString, B.fromStrict, B.toStrict, B.fromChunks, B.toChunks
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Templates.AnyChunks
import SDP.ByteList.IOUblist
import SDP.ByteList.STUblist
import SDP.ByteList.ST
import SDP.SortM.Tim
import SDP.Indexed
import SDP.Sort

import Data.ByteString.Lazy.Internal ( ByteString (..) )
import qualified Data.ByteString.Lazy as B
import qualified SDP.ByteString as S

import Data.Foldable as F ( foldrM )
import Data.Maybe

import Control.Exception.SDP

import System.IO.Classes

default ()

--------------------------------------------------------------------------------

-- | Type synonym to avoid ambiguity.
type LByteString = ByteString

--------------------------------------------------------------------------------

{- Bordered, Linear and Split instances. -}

instance Bordered ByteString Int
  where
    lower      = const 0
    sizeOf     = fromEnum . B.length
    upper   bs = sizeOf bs - 1
    bounds  bs = (0, sizeOf bs - 1)
    indices bs = [0 .. sizeOf bs - 1]
    indexIn bs = \ i -> i >= 0 && i < sizeOf bs
#if MIN_VERSION_sdp(0,3,0)
    rebound    = take . size
#endif

instance Linear ByteString Word8
  where
    replicate   = B.replicate . toEnum
    concat      = B.concat . toList
    fromList    = B.pack
    
    intersperse = B.intersperse
    filter      = B.filter
    
    listR  = \ bs -> let n = sizeOf bs in (bs .!) <$> [n - 1, n - 2 .. 0]
    single = B.singleton
    listL  = B.unpack
    (++)   = B.append
    
    (!^) es = B.index es . toEnum
    
    write bs = (bs //) . single ... (,)
    
    uncons' = B.uncons
    unsnoc' = B.unsnoc
    
    uncons = fromMaybe (pfailEx "uncons") . B.uncons
    unsnoc = fromMaybe (pfailEx "unsnoc") . B.unsnoc
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
        B.foldr (\ b io -> writeM' hs b True >> io) (return ()) bs
        done' hs
      where
        done' :: STByteList s Word8 Bool -> ST s ByteString
        done' =  fmap fromList . kfoldrM (\ i b is -> pure $ b ? (i : is) $ is) []
    
    nubBy f = fromList . B.foldr (\ b es -> any (f b) es ? es $ (b : es)) [] . nub
    
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
    take  = B.take    . toEnum
    drop  = B.drop    . toEnum
    split = B.splitAt . toEnum
    
    isPrefixOf = B.isPrefixOf
    isSuffixOf = B.isSuffixOf
    isInfixOf  = on isInfixOf listL
    
    takeWhile = B.takeWhile
    dropWhile = B.dropWhile
    
    spanl  = B.span
    breakl = B.break

--------------------------------------------------------------------------------

{- Map, Indexed and Sort instances. -}

instance Map ByteString Int Word8
  where
    toMap = toMap' 0
    
    toMap' defvalue ascs = null ascs ? Z $ assoc' (ascsBounds ascs) defvalue ascs
    
    (.!) es = B.index es . toEnum
    
    Z  // ascs = toMap ascs
    es // ascs = assoc (bounds es) (assocs es ++ ascs)
    
    (.$) = fmap fromEnum ... B.findIndex
    (*$) = fmap fromEnum ... B.findIndices
    
    kfoldr = ofoldr
    kfoldl = ofoldl

instance Indexed ByteString Int Word8
  where
    assoc = flip assoc' 0
    
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
    
    fromIndexed es = assoc bnds' [ (offset bnds i, e) | (i, e) <- assocs es ]
      where
        bnds' = defaultBounds (sizeOf es)
        bnds  = bounds es

instance Sort ByteString Word8
  where
    sortBy f bs = runST $ do es' <- thaw bs; timSortBy f es'; done es'
    
    sortedBy _ Empty = True
    sortedBy f (Chunk ch   Z) = sortedBy f ch
    sortedBy f (Chunk Z  chs) = sortedBy f chs
    sortedBy f (Chunk ch chs) = sortedBy f ch && last ch `f` head chs && sortedBy f chs

--------------------------------------------------------------------------------

instance Thaw (ST s) ByteString (STUblist s Word8)
  where
    thaw = fromChunksM <=< B.foldrChunks (liftA2 (:) . thaw) (return [])

instance Freeze (ST s) (STUblist s Word8) ByteString
  where
    freeze = F.foldrM (\ e rs -> (`Chunk` rs) <$> freeze e) Empty . toChunks

instance (MonadIO io) => Thaw io ByteString (MIOUblist io Word8)
  where
    thaw = fromChunksM <=< B.foldrChunks (liftA2 (:) . thaw) (return [])

instance (MonadIO io) => Freeze io (MIOUblist io Word8) ByteString
  where
    freeze = F.foldrM (\ e rs -> (`Chunk` rs) <$> freeze e) Empty . toChunks

--------------------------------------------------------------------------------

{- Nullable, Forceable and Estimate instances. -}

instance Nullable ByteString where lzero = B.empty; isNull = B.null

#if MIN_VERSION_sdp(0,3,0)
instance Forceable ByteString where force = B.copy
#endif

instance Estimate ByteString
  where
    (<==>) = go 0
      where
        go o Empty Empty = o <=> 0
        go o xs    Empty = xs <.=> (-o)
        go o Empty    ys = o <=.> ys
        go o (Chunk ch1 chs1) (Chunk ch2 chs2) =
          go (o + sizeOf ch1 - sizeOf ch2) chs1 chs2
    
    Empty <.=> n = 0 <=> n
    (Chunk ch chs) <.=> n = ch .> n ? GT $ chs <.=> (n - sizeOf ch)

--------------------------------------------------------------------------------

{- IsFile and IsTextFile instances. -}

instance IsFile ByteString
  where
    hGetContents = liftIO  .  B.hGetContents
    hPutContents = liftIO ... B.hPut

instance IsTextFile ByteString
  where
    hPutStrLn h = liftIO  .  (>> hPutChar h '\n') . hPutStr h
    hGetLine    = liftIO  .  fmap B.fromStrict . S.hGetLine
    hPutStr     = liftIO ... B.hPut

--------------------------------------------------------------------------------

ascsBounds :: (Ord a) => [(a, b)] -> (a, a)
ascsBounds =  \ ((x, _) : xs) -> foldr (\ (e, _) (mn, mx) -> (min mn e, max mx e)) (x, x) xs

done :: STUblist s Word8 -> ST s ByteString
done =  freeze

pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.ByteString.Lazy."

lim :: Int
lim =  1024

