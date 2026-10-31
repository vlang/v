# crypto.blake2b

`Digest.checksum()` returns a snapshot of the current hash without modifying the digest.
Repeated calls return the same bytes, and subsequent `write()` calls continue hashing.
This applies to keyed and unkeyed digests.
