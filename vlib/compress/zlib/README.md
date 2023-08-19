## Description:

`compress.zlib` is a module that assists in the compression and
decompression of binary data using `zlib` compression

## Examples:

```v
import compress.zlib

fn main() {
	uncompressed := 'Hello world!'
	compressed := zlib.pack(uncompressed.bytes())!
	decompressed := zlib.unpack(compressed)!
	assert decompressed == uncompressed.bytes()
}
```
