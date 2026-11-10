## Description

`compress.deflate` is a pure V DEFLATE compression and decompression module.

Compression output format is selected by `CompressFormat` via
`compress(data, format: ...)`:

- `.zlib` (RFC 1950 wrapper)
- `.gzip` (RFC 1952 wrapper)
- `.raw_deflate` (RFC 1951 raw stream)

`compress` keeps default zlib behavior, and `decompress` auto-detects all three.

The effort spent looking for matches is set by `level`, from 0 to 9, with the same meaning as
in zlib: 0 stores the data, 6 is the default, 9 searches hardest. Each block is written as
stored, fixed Huffman or dynamic Huffman, whichever costs the fewest bits, so data that does
not compress is stored rather than grown.

```v
import compress.deflate

fn main() {
	data := 'hello '.repeat(100).bytes()
	small := deflate.compress(data, format: .raw_deflate, level: 9)!
	fast := deflate.compress(data, format: .raw_deflate, level: 1)!
	assert small.len <= fast.len
	assert deflate.decompress(small)! == data
}
```

A program compressing many small buffers can keep one `Compressor`, which reuses its match
finder and Huffman tables instead of allocating them for every call:

```v
import compress.deflate

fn main() {
	mut c := deflate.new_compressor(6)!
	for part in ['first', 'second', 'third'] {
		raw := c.compress_raw(part.repeat(10).bytes())!
		assert deflate.decompress(raw)! == part.repeat(10).bytes()
	}
}
```

## Testing

Run the module's roundtrip, stream, checksum and malformed-input tests from the repository root:

```bash
./v test vlib/compress/deflate/
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

Gzip decompression reads all concatenated members and validates each member separately.

A callback returning zero aborts decompression immediately. The returned count covers delivered
bytes; unread compressed data and container checksums are not validated after an abort.

Inflation rejects dynamic blocks without an end-of-block code, invalid or incomplete Huffman
codes, and code-length repeats beyond the declared alphabets. A literals-only block may omit
distance codes; using a length symbol in such a block is an error. The same validation applies
to streaming inflation.

Dynamic header validation follows zlib's strict policy: literal/length counts above 286 and
distance counts above 30 are rejected. Although the RFC 1951 header can encode 31 or 32 distance
codes, these counts are rejected even when the reserved symbols would not appear in the data.

Stored blocks following Huffman blocks retain any complete bytes read ahead by the Huffman
decoder. With an otherwise intact zlib or gzip stream, checksum-only corruption is reported
as an Adler-32, CRC-32 or uncompressed-size mismatch by one-shot decoding and callbacks that
read the whole stream.

`validate_zlib_header` accepts the two-byte RFC 1950 header and rejects compression
info values above 7. Decompression separately rejects streams shorter than the header
and Adler-32 trailer, including the callback entry point.
