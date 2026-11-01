# ascii85

`ascii85` encodes and decodes binary data using ASCII85 (Base85), as used by
Go's `encoding/ascii85` and Python's `base64.a85encode`.

`encode` produces raw ASCII85 without delimiters or whitespace. A complete group
of four zero bytes is written as `z`; partial groups use two to four characters.

`decode` ignores ASCII spaces, tabs, line feeds, carriage returns, form feeds,
and vertical tabs. It rejects invalid characters, a `z` inside a group, trailing
groups of only one character, and groups whose value exceeds 2^32 - 1, including
padded partial groups. Adobe `<~` / `~>` delimiters and the `y` space shortcut
are not supported.

```v
import encoding.ascii85

enc := ascii85.encode('Hello, World!'.bytes())
assert ascii85.decode(enc)! == 'Hello, World!'.bytes()
```
