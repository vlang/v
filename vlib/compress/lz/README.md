## Description

`compress.lz` provides pure V implementations of several LZ-family codecs.

Supported formats:

- `lz77`
- `lz78`
- `lzw`
- `lz4`
- `lzss`
- `lzma`
- `lzjb`

Use the generic API when selecting a format dynamically:

```v
import compress.lz

encoded := lz.compress('hello hello hello'.bytes(), .lz77)!
decoded := lz.decompress(encoded, .lz77)!
assert decoded.bytestr() == 'hello hello hello'
```

Use the format-specific APIs for direct calls:

```v
import compress.lz

encoded := lz.compress_lzw('banana banana'.bytes())!
decoded := lz.decompress_lzw(encoded)!
assert decoded.bytestr() == 'banana banana'
```

