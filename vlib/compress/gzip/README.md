## Description

`compress.gzip` is a module that assists in the compression and
decompression of binary data using `gzip` compression.

`decompress` and `decompress_with_callback` read every member in a concatenated gzip stream.
Each member's header, checksum, and size are validated. A truncated trailing member is an error.
Returning zero from the callback stops decoding, including any remaining members.


## Examples:

```v
import compress.gzip

fn main() {
	uncompressed := 'Hello world!'
	compressed := gzip.compress(uncompressed.bytes())!
	decompressed := gzip.decompress(compressed)!
	assert decompressed == uncompressed.bytes()
}
```
