## Description

Pure V Brotli compression and decompression, with no system library
dependency. `decompress_with_callback` decodes a stream and delivers
the output in 32 KiB chunks; returning a short count from the callback
stops decoding, skips the remaining input validation, and returns the
bytes delivered so far without an error. The chunk slice is only valid
for the duration of the callback.

## Examples

```v oksyntax
import x.compress.brotli

fn main() {
	compressed := brotli.compress('Hello world!'.bytes())!
	mut out := []u8{}
	mut ref := &out
	delivered := brotli.decompress_with_callback(compressed, fn (chunk []u8, ref &[]u8) int {
		unsafe {
			*ref << chunk
		}
		return chunk.len
	}, ref)!
	assert delivered == out.len
}
```
