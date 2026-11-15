# utf32

`utf32` encodes and decodes UTF-32 (one u32 per code point).

Use `encode` and `decode` for `[]u32` code units. Use `encode_le`, `encode_be`,
`decode_le`, and `decode_be` for little-endian or big-endian byte sequences.
The byte decoders return an error when the input length is not a multiple of four.

`decode` and the byte decoders replace surrogates (U+D800 through U+DFFF) and
values above U+10FFFF with U+FFFD. `encode` replaces malformed UTF-8 in the same
way as `string.runes()`.

The endian-specific functions do not add, remove, or detect a byte order mark.
U+FEFF and embedded NUL characters are preserved, and the selected function
determines the byte order.

```v
import encoding.utf32

units := utf32.encode('V 😀')
assert utf32.decode(units) == 'V 😀'
assert utf32.encode_le('A') == [u8(0x41), 0, 0, 0]
assert utf32.decode_be([u8(0), 0, 0, 0x41])! == 'A'
```
