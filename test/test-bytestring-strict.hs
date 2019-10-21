module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import SDP.ByteString
import SDP.ByteString.Related ()

import Test.SDP

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [
    -- common tests
    testProperty "strict-bytestring-eq             " eqProp,
    testProperty "strict-bytestring-ord            " ordProp,
    testProperty "strict-bytestring-lexicographic  " lgoProp,
    
    -- linear tests
    testProperty "strict-bytestring-linear-basic   " basicLinearProp,
    testProperty "strict-bytestring-linear-decons  " deconstructionLinearProp,
    testProperty "strict-bytestring-linear-cons    " constructionLinearProp,
    testProperty "strict-bytestring-linear-reverse " reverseProp,
    testProperty "strict-bytestring-linear-concat  " concatProp,
    
    -- split test
    testProperty "strict-bytestring-split          " splitProp,
    
    -- indexed tests
    testProperty "strict-bytestring-indexed-basic  " basicIndexedProp,
    testProperty "strict-bytestring-indexed-assoc  " assocIndexedProp,
    testProperty "strict-bytestring-indexed-read   " readIndexedProp,
    
    -- sort test
    testProperty "strict-bytestring-sort           " sortProp,
    
    -- set test (planned)
    
    -- estimate test
    testProperty "strict-bytestring-estimate       " estimateProp
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


