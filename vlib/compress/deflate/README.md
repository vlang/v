## Description:

`compress.deflate` is a module that assists in the compression and
decompression of binary data using `deflate` compression

NOTE: To decompress gzip, discard first 10 bytes of compressed bytes then use `compress.deflate.decompress`. (Header validation doesn't perform in this case)

## Examples:

```v
import compress.deflate

fn main() {
	uncompressed := 'Hello world!'
	compressed := deflate.compress(uncompressed.bytes())?
	decompressed := deflate.decompress(compressed)?
	assert decompressed == uncompressed.bytes()
}
```