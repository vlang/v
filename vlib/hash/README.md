## Description

`hash` provides a way to hash binary data, i.e. produce a shorter value,
that is highly content dependent, so even slightly different content will
produce widely different hashes.

Hash functions are useful for implementing maps, caches etc.

## Available submodules

- `hash.adler32` - Adler-32 (RFC 1950 checksum used by zlib)
- `hash.crc32` - CRC-32 variants (IEEE 802.3 `crc32`, Castagnoli `crc32c`, Koopman `crc32k`,
  CRC-32Q `crc32q`)
- `hash.crc64` - CRC-64-ECMA-182
- `hash.fnv1a` - Fowler-Noll-Vo hashes
