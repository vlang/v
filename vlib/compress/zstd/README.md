## Description

`compress.zstd` is a module that assists in the compression and
decompression of binary data using `zstd` compression.

## Examples

Decode a `.zst` file from disk:

```v
import os
import compress.zstd

fn main() {
	compressed := os.read_bytes('document.zst') or { panic(err) }
	decompressed := zstd.decompress(compressed) or { panic(err) }
	println(decompressed.bytestr())
}
```

For a runnable example in this repository, see
[examples/read_zstd_file.v](examples/read_zstd_file.v).

`read_zstd_files_test.v` is a test file. Run it with
`v test read_zstd_files_test.v`, not with `v read_zstd_files_test.v`.
