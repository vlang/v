# utf16

`utf16` encodes and decodes UTF-16 code units, including surrogate pairs.

Use `encode`/`decode` when you already have `[]u16` code units (for example,
from a language or format that exposes UTF-16 text as 16-bit units). Use
`encode_le`, `encode_be`, `decode_le` and `decode_be` for the byte-oriented
UTF-16LE and UTF-16BE representations used by files and network protocols.

Invalid code points are encoded as U+FFFD. `decode` is lenient and replaces
invalid surrogate sequences with U+FFFD, matching the behaviour of Go's
`unicode/utf16`; `decode_strict` returns an error instead.

```v
import encoding.utf16

units := utf16.encode('V 😀')
assert utf16.decode(units) == 'V 😀'
assert utf16.encode_le('A') == [u8(0x41), 0]
assert utf16.decode_be([u8(0), 0x41]) or { panic(err) } == 'A'
```
