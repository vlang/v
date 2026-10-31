# utf32

`utf32` encodes and decodes UTF-32 (one u32 per code point).

```v
import encoding.utf32

units := utf32.encode('V 😀')
assert utf32.decode(units) == 'V 😀'
assert utf32.encode_le('A') == [u8(0x41), 0, 0, 0]
```
