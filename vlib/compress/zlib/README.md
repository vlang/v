## Description:

`compress.zlib` implements zlib compression and decompression of binary data.

## Examples:

```v
import compress.zlib

fn main() {
	uncompressed := 'Hello world!'
	compressed := zlib.compress(uncompressed.bytes()) ?
	decompressed := zlib.decompress(compressed) ?
	assert decompressed == uncompressed.bytes()
}
```
