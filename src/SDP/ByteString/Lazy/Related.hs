{- |
    Module      :  SDP.ByteString.Lazy.Related
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    @SDP.ByteString.Lazy.Related@ is service module that contains 'Default' and
    'Arbitrary' instances for strict 'ByteString' (this implementations do not
    require installation of additional libraries because SDP depends on
    data-default and QuickCheck).
    
    If you use bytestrings with other structures, use quickcheck-instances and
    data-default-instances-bytestring.
-}
module SDP.ByteString.Lazy.Related ( module SDP.ByteString.Lazy ) where

import Prelude ()
import SDP.SafePrelude

import SDP.ByteString.Lazy

import Test.QuickCheck

import Data.Default

default ()

--------------------------------------------------------------------------------

instance Default   ByteString where def = Z

instance Arbitrary ByteString where arbitrary = fromList <$> arbitrary


