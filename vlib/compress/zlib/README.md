## Description:

`compress.zlib` is a module that assists in the compression and decompression of binary data using `zlib` compression

Currently, only `zlib` compression is supported.

## Examples:

```v
import compress.zlib

// usually, you will read a file to get an array of bytes
// this trivial example just initializes a 1 KB array with 1s
b := []byte{len: 1024, init: 1}
compressed := zlib.compress(b) ?
assert compressed.len < b.len

d := zlib.decompress(compressed) ?
assert d == b

```
