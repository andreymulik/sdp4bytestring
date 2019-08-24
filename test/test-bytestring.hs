module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import SDP.ByteString
import SDP.ByteString.Related ()

import Test.SDP.Indexed
import Test.SDP.Linear
import Test.SDP.Sort

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [
    -- linear tests
    testProperty "bytestring-linear-basic   " basicLinearProp,
    testProperty "bytestring-linear-decons  " deconstructionLinearProp,
    testProperty "bytestring-linear-cons    " constructionLinearProp,
    testProperty "bytestring-linear-reverse " reverseProp,
    testProperty "bytestring-linear-concat  " concatProp,
    
    -- split test
    testProperty "bytestring-split          " splitProp,
    
    -- indexed tests
    testProperty "bytestring-indexed-basic  " basicIndexedProp,
    testProperty "bytestring-indexed-assoc  " assocIndexedProp,
    testProperty "bytestring-indexed-read   " readIndexedProp,
    
    -- sort test
    testProperty "bytestring-sort           " sortProp
    
    -- set test (planned)
  ]

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

sortProp :: ByteString -> Bool
sortProp =  sortTest


