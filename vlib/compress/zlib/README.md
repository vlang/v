## Description

`compress.zlib` is a small wrapper around `compress.deflate` for working with zlib streams
(RFC 1950).

## Examples

```v
import compress.zlib

fn main() {
	data := 'Hello world!'.bytes()
	compressed := zlib.compress(data) or { panic(err) }
	decompressed := zlib.decompress(compressed) or { panic(err) }
	assert decompressed == data
}
```
