## Description:

`compress.deflate` is a module that assists in the compression and
decompression of binary data using `deflate` compression


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