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
    testProperty "linear-basic   " basicLinearProp,
    testProperty "linear-decons  " deconstructionLinearProp,
    testProperty "linear-cons    " constructionLinearProp,
    testProperty "linear-reverse " reverseProp,
    testProperty "linear-concat  " concatProp,
    
    -- split test
    testProperty "split          " splitProp,
    
    -- indexed tests
    testProperty "indexed-basic  " basicIndexedProp,
    testProperty "indexed-assoc  " assocIndexedProp,
    testProperty "indexed-read   " readIndexedProp,
    
    -- sort test
    testProperty "sort           " sortProp
    
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


