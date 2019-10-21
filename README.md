# sdp4bytestring

SDP to ByteString is an implementation of the core SDP classes for ByteString.

## Reasons

ByteString is one of the best libraries on the Haskell Platform - it combines
simplicity, versatility, and efficiency. ByteString used in an impressive number
of various projects. For these and some other reasons, I chose ByteString as the
first library that SDP will support through extensions.

## Versioning

sdp4bytestring follows of the Haskell community versioning principles (with some
self-restraints). In the version number a.b.c.d:
* d is the patch number, also can be used to mark changes in dependencies
(in .cabal file)
* c is internal wrapper version number, used to mark changes in the API and code
* b is the stable version number, if (by a lucky chance) when updating the SDP,
the wrapper compiles and works correctly, the number remains the same.
* a is not used (always 0) - it's necessary for the wrapper version to match the
SDP version.



