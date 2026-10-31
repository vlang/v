# ascii85

`ascii85` encodes and decodes binary data using ASCII85 (Base85), as used by
Go's `encoding/ascii85` and Python's `base64.a85encode`.

```v
import encoding.ascii85

enc := ascii85.encode('Hello, World!'.bytes())
assert ascii85.decode(enc)! == 'Hello, World!'.bytes()
```
