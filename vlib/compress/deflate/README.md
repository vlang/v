## Description

`compress.deflate` is a pure V RFC-compliant DEFLATE module.

Compression output format is selected by `CompressFormat` via
`compress(data, format: ...)`:

- `.zlib` (RFC 1950 wrapper)
- `.gzip` (RFC 1952 wrapper)
- `.raw_deflate` (RFC 1951 raw stream)

`compress` keeps default zlib behavior, and `decompress` auto-detects all three.

## Interop Validation

Cross-validation with C/zlib is kept separate from `v test` and can be run manually:

```bash
./vnew run vlib/compress/deflate/interop/deflate_interop.v
```

## Example

```v
import compress.deflate

fn main() {
	uncompressed := 'Hello world!'
	zlib_stream := deflate.compress(uncompressed.bytes())!
	gzip_stream := deflate.compress(uncompressed.bytes(), format: .gzip)!
	raw_stream := deflate.compress(uncompressed.bytes(), format: .raw_deflate)!
	assert deflate.decompress(zlib_stream)! == uncompressed.bytes()
	assert deflate.decompress(gzip_stream)! == uncompressed.bytes()
	decompressed := deflate.decompress(raw_stream)!
	assert decompressed == uncompressed.bytes()
}
```
