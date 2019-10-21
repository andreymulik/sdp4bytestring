module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import SDP.ByteString.Lazy
import SDP.ByteString.Lazy.Related ()

import Test.SDP

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [
    -- common tests
    testProperty "lazy-bytestring-eq             " eqProp,
    testProperty "lazy-bytestring-ord            " ordProp,
    testProperty "lazy-bytestring-lexicographic  " lgoProp,
    
    -- linear tests
    testProperty "lazy-bytestring-linear-basic   " basicLinearProp,
    testProperty "lazy-bytestring-linear-decons  " deconstructionLinearProp,
    testProperty "lazy-bytestring-linear-cons    " constructionLinearProp,
    testProperty "lazy-bytestring-linear-reverse " reverseProp,
    testProperty "lazy-bytestring-linear-concat  " concatProp,
    
    -- split test
    testProperty "lazy-bytestring-split          " splitProp,
    
    -- indexed tests
    testProperty "lazy-bytestring-indexed-basic  " basicIndexedProp,
    testProperty "lazy-bytestring-indexed-assoc  " assocIndexedProp,
    testProperty "lazy-bytestring-indexed-read   " readIndexedProp,
    
    -- sort test
    testProperty "lazy-bytestring-sort           " sortProp,
    
    -- set test (planned)
    
    -- estimate test
    testProperty "lazy-bytestring-estimate       " estimateProp
  ]

--------------------------------------------------------------------------------

{- Eq property. -}

eqProp :: TestEq ByteString
eqProp =  eqTest

--------------------------------------------------------------------------------

{- Ord property. -}

ordProp :: TestOrd ByteString
ordProp =  ordTest

lgoProp :: Long ByteString -> Long ByteString -> Bool
lgoProp (Long xs) (Long ys) = lexicographicOrdTest xs ys

--------------------------------------------------------------------------------

{- Linear properties. -}

basicLinearProp          :: Word8 -> ByteString -> Bool
basicLinearProp          =  basicLinearTest

deconstructionLinearProp :: ByteString -> Bool
deconstructionLinearProp =  deconstructionLinearTest

constructionLinearProp   :: Word8 -> ByteString -> Bool
constructionLinearProp   =  constructionLinearTest

reverseProp              :: ByteString -> Bool
reverseProp              =  reverseTest

replicateProp            :: TestLinear ByteString Word8
replicateProp            =  replicateTest

concatProp               :: ByteString -> Bool
concatProp               =  concatTest

--------------------------------------------------------------------------------

{- Split property. -}

splitProp :: TestSplit ByteString
splitProp =  splitTest

--------------------------------------------------------------------------------

{- Indexed property. -}

basicIndexedProp :: TestIndexed ByteString Int
basicIndexedProp =  basicIndexedTest

assocIndexedProp :: TestIndexed ByteString Int
assocIndexedProp =  assocIndexedTest

readIndexedProp  :: TestIndexed ByteString Int
readIndexedProp  =  readIndexedTest

--------------------------------------------------------------------------------

{- Sort property. -}

sortProp :: Medium ByteString -> Bool
sortProp =  sortTest

--------------------------------------------------------------------------------

{- Estimate property. -}

estimateProp :: TestEstimate ByteString
estimateProp =  estimateTest


