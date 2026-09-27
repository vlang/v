## Description

`compress.brotli` compresses and decompresses binary data using Brotli.

The module loads the system `libbrotlienc` and `libbrotlidec` shared
libraries at runtime. Set `V_BROTLI_ENC_LIB` or `V_BROTLI_DEC_LIB` to point to
custom library locations when they are not in the platform loader path.

## Examples

```v oksyntax
import compress.brotli

fn main() {
	uncompressed := 'Hello world!'
	compressed := brotli.compress(uncompressed.bytes(), mode: .text)!
	decompressed := brotli.decompress(compressed)!
	assert decompressed == uncompressed.bytes()
}
```
