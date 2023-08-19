## Description:

`compress.gzip` is a module that assists in the compression and
decompression of binary data using `gzip` compression


## Examples:

```v
import compress.gzip

fn main() {
	uncompressed := 'Hello world!'
	compressed := gzip.pack(uncompressed.bytes())!
	decompressed := gzip.unpack(compressed)!
	assert decompressed == uncompressed.bytes()
}
```
